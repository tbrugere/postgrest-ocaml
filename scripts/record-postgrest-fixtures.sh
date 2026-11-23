#!/usr/bin/env bash
# Replay test/fixtures/scenarios.spec.json against a throwaway
# Postgres + PostgREST (Docker) and record the real responses into
# test/fixtures/scenarios.json.
#
#   scripts/record-postgrest-fixtures.sh [--keep]
#
# --keep leaves the containers running (http://localhost:${PORT:-3000}).
set -euo pipefail
root="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
fixtures="$root/test/fixtures"
net=postgrest-ocaml-pgrst-net
db=postgrest-ocaml-pgrst-db
api=postgrest-ocaml-pgrst
port="${PORT:-3000}"
keep=0
[ "${1:-}" = "--keep" ] && keep=1

cleanup() {
  [ "$keep" = 1 ] && return 0
  docker rm -f "$api" "$db" >/dev/null 2>&1 || true
  docker network rm "$net" >/dev/null 2>&1 || true
}
trap cleanup EXIT
cleanup

docker network create "$net" >/dev/null
docker run -d --name "$db" --network "$net" \
  -e POSTGRES_PASSWORD=pw -e POSTGRES_DB=app postgres:16 >/dev/null
for _ in $(seq 1 60); do
  docker exec "$db" pg_isready -U postgres -d app >/dev/null 2>&1 && break
  sleep 1
done
docker exec -i "$db" psql -U postgres -d app -v ON_ERROR_STOP=1 -q <"$fixtures/schema.sql"
docker exec -i "$db" psql -U postgres -d app -v ON_ERROR_STOP=1 -q <"$fixtures/seed.sql"

docker run -d --name "$api" --network "$net" -p "$port:3000" \
  -e PGRST_DB_URI="postgres://postgres:pw@$db:5432/app" \
  -e PGRST_DB_SCHEMAS=public -e PGRST_DB_ANON_ROLE=web_anon \
  -e PGRST_LOG_LEVEL=info postgrest/postgrest:latest >/dev/null
for _ in $(seq 1 60); do
  curl -sf "http://localhost:$port/" >/dev/null 2>&1 && break
  sleep 1
done
curl -sf "http://localhost:$port/" >/dev/null

pg_version="$(docker exec "$db" postgres --version | tr -d '\n')"
pgrst_version="$(docker inspect --format '{{index .Config.Labels "org.opencontainers.image.version"}}' "$api" 2>/dev/null || true)"
[ -n "$pgrst_version" ] || pgrst_version="$(curl -s "http://localhost:$port/" | python3 -c 'import sys,json; print(json.load(sys.stdin).get("info",{}).get("version","unknown"))' 2>/dev/null || echo unknown)"

export FIXTURES="$fixtures" PORT="$port" DB="$db" PG_VERSION="$pg_version" PGRST_VERSION="$pgrst_version"
python3 - <<'PY'
import json, os, subprocess, urllib.parse, urllib.request, urllib.error

fixtures = os.environ["FIXTURES"]
base = f"http://localhost:{os.environ['PORT']}"
spec = json.load(open(f"{fixtures}/scenarios.spec.json"))

SAFE = ",().:*{}-_~/"   # left literal in query values; everything else (space & = + # % " non-ascii) is encoded

def encode_target(path_query):
    if "?" not in path_query:
        return path_query
    path, query = path_query.split("?", 1)
    parts = []
    for p in query.split("&"):
        k, _, v = p.partition("=")
        parts.append(urllib.parse.quote(k, safe=SAFE) + "=" + urllib.parse.quote(v, safe=SAFE))
    return path + "?" + "&".join(parts)

def reseed():
    subprocess.run(["docker", "exec", "-i", os.environ["DB"], "psql", "-U", "postgres", "-d", "app", "-q", "-v", "ON_ERROR_STOP=1"],
                   stdin=open(f"{fixtures}/seed.sql"), check=True)

WANTED = {"content-type", "content-range", "preference-applied", "location", "content-location"}
out = []
for s in spec["scenarios"]:
    if s.get("reset"):
        reseed()
    target = encode_target(s["path_query"])
    body = None
    if "body" in s:
        body = json.dumps(s["body"], separators=(",", ":")).encode()
    req = urllib.request.Request(base + target, data=body, method=s["method"])
    for k, v in s.get("headers", {}).items():
        req.add_header(k, v)
    try:
        resp = urllib.request.urlopen(req)
        status, hdrs, raw = resp.status, resp.headers, resp.read()
    except urllib.error.HTTPError as e:
        status, hdrs, raw = e.code, e.headers, e.read()
    text = raw.decode("utf-8")
    ctype = (hdrs.get("Content-Type") or "").lower()
    if text and "json" in ctype:
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            parsed = text
    else:
        parsed = text
    out.append({
        "name": s["name"],
        "method": s["method"],
        "path_query": s["path_query"],
        "url_sent": target,
        "request_headers": s.get("headers", {}),
        "request_body": s.get("body"),
        "status": status,
        "response_headers": {k.lower(): v for k, v in hdrs.items() if k.lower() in WANTED},
        "response_body": parsed,
    })
    print(f"{s['name']:34} {status}")

with open(f"{fixtures}/scenarios.json", "w") as f:
    json.dump(out, f, indent=2, sort_keys=True, ensure_ascii=False)
    f.write("\n")

with open(f"{fixtures}/README.md", "w") as f:
    f.write(f"""# PostgREST integration fixtures

`scenarios.json` holds real request/response pairs recorded by
`scripts/record-postgrest-fixtures.sh` against a throwaway PostgREST on
Docker, loaded with `schema.sql` and `seed.sql`. `scenarios.spec.json` is
the list of requests to replay (query strings written unencoded; the
recorder percent-encodes them and stores the exact URL sent).

Recorded with: {os.environ['PG_VERSION']}; PostgREST {os.environ['PGRST_VERSION']}.

Never edit `scenarios.json` by hand: change the spec, schema or seed and
re-run the script.
""")
print(f"wrote {len(out)} scenarios")
PY

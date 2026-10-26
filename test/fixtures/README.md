# PostgREST integration fixtures

`scenarios.json` holds real request/response pairs recorded by
`scripts/record-postgrest-fixtures.sh` against a throwaway PostgREST on
Docker, loaded with `schema.sql` and `seed.sql`. `scenarios.spec.json` is
the list of requests to replay (query strings written unencoded; the
recorder percent-encodes them and stores the exact URL sent).

Recorded with: postgres (PostgreSQL) 16.15 (Debian 16.15-1.pgdg13+2); PostgREST 16.5.

Never edit `scenarios.json` by hand: change the spec, schema or seed and
re-run the script.

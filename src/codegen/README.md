# postgrest-codegen

Generates the `Postgrest.Col` declarations of a database's tables from its
Postgres catalog, so that columns, codecs and relationships are never
written by hand. The catalog is read once by a query, `catalog.sql`, whose
output is the generator's input: the generator itself never connects to a
database. AI-written, see [../AI_POLICY.md](../AI_POLICY.md).

It is its own package, `postgrest-codegen` (library and executable of that
name), so that users of the `postgrest` library do not pull it or ppxlib.
Once published: `opam install postgrest-codegen`; in a dune project, list
it in the package's depends (`:with-test` or a build dependency, as the
rule below runs it) and use `%{bin:postgrest-codegen}`, or link the
library `postgrest-codegen` (module `Postgrest_codegen`).

```
psql "$DB_URL" -X -q -At -v ON_ERROR_STOP=1 -f catalog.sql > catalog.json
postgrest-codegen [--module-prefix MODULE] [-o FILE] catalog.json > tables.ml
```

Writes one OCaml file (stdout, or `FILE`): an abstract type per table, a
module per lookup table generated from its values, a `Col.custom` codec per
typed id or lookup table, then a module per table
with its `table`, one `Col.t` per column and one `Col.relationship` per
foreign key. Exit status 2, with nothing written, on a bad argument, an
unreadable file, or a catalog the rules below do not cover.

## The catalog

`catalog.sql` is plain SQL for any client (psql, a SQL console, a driver):
it creates two helper functions in `pg_temp`, which go away with the
session and leave nothing in the database, then selects one row per table of the schema named in its
`params` CTE (`public`). Each row is a pretty-printed JSON object, so the
output is a stream of JSON values in which a column change is a small diff:

| Field | |
| --- | --- |
| `name`, `comment` | the table's |
| `columns` | in column order, each with `name`, `type` (`format_type`), `not_null`, `has_default`, `is_generated`, `is_primary_key`, `is_unique`, `comment`, `fk_table` |
| `values` | a `kind=lookup` table's `value` column, sorted; `null` for any other table |

Tables are sorted by name, so the committed file diffs well; keys come in
`jsonb`'s order. A foreign key is only reported when it has one column and
targets a table of the same schema (the first by name when a column has
several). An identity `GENERATED ALWAYS` counts as a generated column.
The generator decodes each object with `ppx_deriving_yojson`, strictly: a
field missing, mistyped or unknown is an error. It links neither `postgrest`
nor a database driver; its opam package depends on `postgrest` of the same
version only because the code it prints targets that version's API.

Postgres types map to `Col.base`: `text` and `uuid` (without `id=`) → `Str`, `integer` → `Integer`,
`double precision` → `Numeric`, `boolean` → `Bool`,
`timestamp with time zone` → `TimestampTz`, `jsonb` and `text[]` → `JSON`.
`NOT NULL` gives `Not_Null`, otherwise `Nullable`; a column is `` `Mandatory ``
when it is `NOT NULL` without a default.

## Metadata

What SQL cannot say goes in a `metadata:` line of the table or column
comment: `key=value` pairs and bare flags, separated by commas or spaces.

| On | Metadata | Effect |
| --- | --- | --- |
| table | `kind=lookup` | A lookup table: no table module; a module named after the table (`genres` → `Genres`) is generated from its values, and a column with a foreign key to it is typed `Genres.t`. |
| table | `kind=lookup, ocaml=<Module>` | The same, but `<Module>` is written by hand and nothing is generated; the column is typed `<Module>.t`. |
| table | `id=<Module>` | The `uuid` primary key, and every `uuid` foreign key to it, is typed `<Module>.t`. Without it, and for any other `uuid` column, a `uuid` is a `string`. |
| column | `server_only` | Left out: the client never reads or writes it. |
| column | `no_reverse` | No `Many` back from the referenced table. |

`<Module>` is a module path, under `--module-prefix` when given, with
`type t`, `to_json : t -> Yojson.Safe.t` and
`of_json : Yojson.Safe.t -> t option`.

A generated lookup module is not under `--module-prefix` (it is in the
generated file). It has one constant constructor per value, the value
capitalised, in the catalog's sorted order:

```ocaml
module Genres = struct
  type t = Essay | Novel | Poetry
  let all : t list = [Essay; Novel; Poetry]
  let to_string : t -> string = function Essay -> "essay" | ...
  let of_string : string -> t option = function "essay" -> Some Essay | ... | _ -> None
  let to_json : t -> Yojson.Safe.t = fun t -> `String (to_string t)
  let of_json : Yojson.Safe.t -> t option = function `String s -> of_string s | _ -> None
end
```

Its values are read when the catalog is dumped: adding a value to the table
needs a new dump and a regeneration, as a column change does.

```sql
comment on table public.plants is 'A plant of the collection.
metadata: id=Ids.Plant_id';
comment on column public.plants.owner_id is 'metadata: server_only';
```

A foreign key `x_id` to another generated table gives a relationship `x`
(`One` if nullable, `Exactly_one` if `NOT NULL`; named after the referenced
table when the column has no `_id`), and the referenced table a `Many`
named after this table.

## What is rejected

Only what would generate wrong OCaml, or an embed PostgREST finds
ambiguous (`Error.t`): a value that is not JSON or not a table object
(with `ppx_deriving_yojson`'s path to the bad field), a
malformed `metadata:` line or an unknown key, an unsupported type, a lookup table without
`ocaml=` that has no values or a value that is not a constructor once
capitalised (a letter, then letters, digits, `_` or `'`), a generated column, two foreign keys between
the same two tables (`server_only` ones included), and a unique foreign key
without `no_reverse`. Multi-column foreign keys, and a second foreign key
on one column, are not reported, so not checked for ambiguity either. Names
that are not OCaml identifiers, or that clash, are left to the compiler:
the output is compiled anyway.

## As a dune rule

The generated file is committed; a rule regenerates it and `dune test`
fails when the two differ, `dune promote` updates it:

```
(rule
 (deps %{project_root}/db/catalog.json)
 (action
  (with-stdout-to tables.ml.gen
   (run %{bin:postgrest-codegen} --module-prefix My_domain %{deps}))))

(rule
 (alias runtest)
 (action (diff tables.ml tables.ml.gen)))
```

The output starts with `[@@@ocamlformat "disable"]`: the generator owns its
layout, so ocamlformat neither checks nor rewrites it. That layout is
ppxlib's Pprintast, so a ppxlib upgrade can change the file byte for byte:
after one, `dune promote` the new output.

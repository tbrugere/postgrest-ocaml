The executable over a one-table catalog, as catalog.sql writes it, on stdout:

  $ cat > catalog.json <<'JSON'
  > {
  >     "name": "things",
  >     "values": null,
  >     "columns": [
  >         {
  >             "name": "id",
  >             "type": "uuid",
  >             "comment": null,
  >             "fk_table": null,
  >             "not_null": true,
  >             "is_unique": true,
  >             "has_default": false,
  >             "is_generated": false,
  >             "is_primary_key": true
  >         },
  >         {
  >             "name": "label",
  >             "type": "text",
  >             "comment": null,
  >             "fk_table": null,
  >             "not_null": false,
  >             "is_unique": false,
  >             "has_default": false,
  >             "is_generated": false,
  >             "is_primary_key": false
  >         }
  >     ],
  >     "comment": "metadata: id=Thing_id"
  > }
  > JSON
  $ postgrest-codegen --module-prefix My_domain catalog.json | sed -n '/^\[@@@/,$p'
  [@@@ocamlformat "disable"]
  
  module Col = Postgrest.Col
  
  type things
  
  let thing_id : My_domain.Thing_id.t Col.custom =
    {
      name = "uuid";
      to_json = My_domain.Thing_id.to_json;
      of_json = My_domain.Thing_id.of_json
    }
  
  module Things =
    struct
      type t = things
      let table : t Col.table = Col.table "things"
      let id : (t, My_domain.Thing_id.t, [ `Mandatory ]) Col.t =
        { ty = (Not_Null (Custom thing_id)); name = "id" }
      let label : (t, string option, [ `Optional ]) Col.t =
        { ty = (Nullable Str); name = "label" }
    end

Without id=, the uuid key gets a generated Things_id module, unless
--no-generate-ids turns that default off:

  $ sed 's/"metadata: id=Thing_id"/null/' catalog.json > plain.json
  $ postgrest-codegen plain.json | grep -E '^module Things|let id'
  module Things_id :
  module Things =
      let id : (t, Things_id.t, [ `Mandatory ]) Col.t =
  $ postgrest-codegen --no-generate-ids plain.json | grep -E '^module Things|let id'
  module Things =
      let id : (t, string, [ `Mandatory ]) Col.t =

-o writes the same text to a file:

  $ postgrest-codegen --module-prefix My_domain -o things.ml catalog.json
  $ postgrest-codegen --module-prefix My_domain catalog.json | cmp - things.ml

A catalog the rules do not cover exits 2 and writes nothing:

  $ sed 's/"text"/"point"/' catalog.json > bad.json
  $ postgrest-codegen -o bad.ml bad.json
  error: bad.json: column things.label: unsupported type point
  [2]
  $ test -e bad.ml
  [1]

  $ printf '{\n  "name" "things"\n}\n' > broken.json
  $ postgrest-codegen broken.json
  error: broken.json: Line 2, bytes 9-20: Expected ':' but found '"things" } '
  [2]
  $ printf '{"name": "things", "comment": null}\n' > partial.json
  $ postgrest-codegen partial.json
  error: partial.json: value 1: not a table object: Catalog.table.values
  [2]

So do an unreadable file, no file, two files and a bad argument:

  $ postgrest-codegen missing.json
  error: missing.json: No such file or directory
  [2]
  $ postgrest-codegen
  error: give exactly one catalog file
  [2]
  $ postgrest-codegen catalog.json catalog.json
  error: give exactly one catalog file
  [2]
  $ postgrest-codegen --bogus 2>&1 | head -1
  postgrest-codegen: unknown option '--bogus'.
  $ postgrest-codegen --bogus catalog.json > /dev/null 2>&1
  [2]

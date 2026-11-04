(* Tests for postgrest-codegen: catalog table objects built for each decoding,
   typing and relationship rule and each error, and the emitted items
   compared, as ASTs, with hand-written OCaml. *)

open Postgrest_codegen

(* ---------- catalog objects ---------- *)

let opt = function None -> `Null | Some s -> `String s

let column ?(not_null = false) ?(default = false) ?(generated = false)
    ?(pk = false) ?(unique = false) ?comment ?fk name sql_type : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String name);
      ("type", `String sql_type);
      ("not_null", `Bool not_null);
      ("has_default", `Bool default);
      ("is_generated", `Bool generated);
      ("is_primary_key", `Bool pk);
      ("is_unique", `Bool (unique || pk));
      ("comment", opt comment);
      ("fk_table", opt fk);
    ]

type table = {
  name : string;
  comment : string option;
  columns : Yojson.Safe.t list;
  values : string list option;
}

let table ?comment ?values name columns = {name; comment; columns; values}

let to_json t : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String t.name);
      ("comment", opt t.comment);
      ("columns", `List t.columns);
      ( "values",
        match t.values with
        | None -> `Null
        | Some values -> `List (List.map (fun v -> `String v) values) );
    ]

(* Pretty-printed and one after the other, as catalog.sql writes them. *)
let text tables =
  String.concat "\n"
    (List.map (fun t -> Yojson.Safe.pretty_to_string (to_json t)) tables)
  ^ "\n"

let id = column ~not_null:true ~pk:true "id" "uuid"

let owner =
  column ~not_null:true ~default:true ~comment:"metadata: server_only"
    "owner_id" "uuid"

let note_item =
  column "item_id" "uuid" ~not_null:true ~fk:"items"
    ~comment:"The item noted.\nmetadata: no_reverse"

let fixture =
  [
    table "items" ~comment:"Items.\nmetadata: id=Ids.Item_id"
      [
        id;
        owner;
        column "parent_id" "uuid" ~fk:"parents";
        column "name" "text" ~not_null:true;
        column "seen_at" "timestamp with time zone";
        column "source" "text" ~not_null:true ~fk:"sources";
        column "tags" "text[]" ~not_null:true;
        column "payload" "jsonb" ~not_null:true;
        column "count" "integer" ~not_null:true ~default:true;
        column "score" "double precision";
        column "active" "boolean" ~not_null:true;
        column "loc" "extensions.geography(Point,4326)"
          ~comment:"Set by a trigger.\nmetadata: server_only";
      ];
    table "notes" ~comment:"metadata: id=Ids.Note_id"
      [id; note_item; column "body" "text" ~not_null:true];
    table "parents" ~comment:"Parents.\nmetadata: id=Ids.Parent_id"
      [id; owner; column "label" "text" ~not_null:true];
    table "sources"
      ~comment:"Where from.\nmetadata: kind=lookup, ocaml=Thing.Source"
      ~values:["a"; "b"]
      [
        column "value" "text" ~not_null:true ~pk:true;
        column "description" "text";
      ];
  ]

(* The fixture with the [(table, column)] columns added to its tables, every
   column equal to one of [drop] left out, and [tables] added. *)
let edited ?(drop = []) ?(tables = []) columns =
  List.map
    (fun t ->
      let kept = List.filter (fun c -> not (List.mem c drop)) t.columns in
      let added =
        List.filter_map
          (fun (name, c) ->
            match String.equal name t.name with true -> Some c | false -> None)
          columns
      in
      {t with columns = kept @ added})
    fixture
  @ tables

let fail_with e = Alcotest.fail (Error.to_string e)

let parse text =
  match Catalog.of_string text with Ok c -> c | Error e -> fail_with e

let catalog tables = parse (text tables)

let model ?(module_prefix = ["Domain"]) ?(tables = fixture) () =
  match Model.of_catalog ~module_prefix (catalog tables) with
  | Ok m -> m
  | Error e -> fail_with e

let error = Alcotest.testable (Fmt.of_to_string Error.to_string) ( = )

let check_text_error text expected =
  Alcotest.(check (result reject error))
    (Error.to_string expected) (Error expected)
    (Result.bind (Catalog.of_string text) (Model.of_catalog ~module_prefix:[]))

let check_error tables expected = check_text_error (text tables) expected

(* ---------- catalog ---------- *)

let catalog_tests =
  let open Alcotest in
  let table_of name (c : Catalog.t) =
    match List.find_opt (fun (t : Catalog.table) -> t.name = name) c with
    | Some t -> t
    | None -> failf "no table %s" name
  in
  [
    test_case "table objects" `Quick (fun () ->
        let c = catalog fixture in
        check (list string) "tables"
          ["items"; "notes"; "parents"; "sources"]
          (List.map (fun (t : Catalog.table) -> t.name) c);
        let items = table_of "items" c in
        check int "items columns" 12 (List.length items.columns);
        check (option (list string)) "items values" None items.values;
        check
          (option (list string))
          "lookup values"
          (Some ["a"; "b"])
          (table_of "sources" c).values;
        match items.columns with
        | first :: _ ->
            check bool "id" true
              (first
              = {
                  name = "id";
                  sql_type = "uuid";
                  not_null = true;
                  has_default = false;
                  is_generated = false;
                  is_primary_key = true;
                  is_unique = true;
                  comment = None;
                  foreign_key = None;
                })
        | [] -> fail "no column");
    test_case "compact values, with or without whitespace between" `Quick
      (fun () ->
        let compact = List.map (fun t -> Yojson.Safe.to_string (to_json t)) in
        check int "no whitespace" 4
          (List.length (parse (String.concat "" (compact fixture))));
        check int "blank lines" 4
          (List.length
             (parse ("\n\n" ^ String.concat "\n\n" (compact fixture) ^ "\n\n"))));
    test_case "empty" `Quick (fun () ->
        check int "tables" 0 (List.length (parse "")));
    test_case "not JSON" `Quick (fun () ->
        match
          Catalog.of_string
            (Yojson.Safe.to_string (to_json (table "a" [])) ^ "\n{")
        with
        | Error (Bad_json message) ->
            check bool message true
              (String.starts_with ~prefix:"Line 2," message)
        | Ok _ | Error _ -> fail "expected Bad_json on line 2");
    test_case "missing field" `Quick (fun () ->
        check_text_error {|{"name":"a","comment":null,"columns":[]}|}
          (Bad_table {index = 1; path = "Catalog.table.values"}));
    test_case "not an object" `Quick (fun () ->
        check_text_error
          (text [table "a" []] ^ "[]")
          (Bad_table {index = 2; path = "Catalog.table"}));
    test_case "a column's field mistyped" `Quick (fun () ->
        check_text_error
          (text
             [
               table "a" [];
               table "b"
                 [
                   (match column "x" "text" with
                   | `Assoc fields ->
                       `Assoc
                         (List.map
                            (function
                              | "not_null", _ -> ("not_null", `String "yes")
                              | field -> field)
                            fields)
                   | other -> other);
                 ];
             ])
          (Bad_table {index = 2; path = "Catalog.column.not_null"}));
    test_case "columns not a list" `Quick (fun () ->
        check_text_error
          {|{"name":"a","comment":null,"columns":{},"values":null}|}
          (Bad_table {index = 1; path = "Catalog.table.columns"}));
    test_case "values not a list of strings" `Quick (fun () ->
        check_text_error
          {|{"name":"a","comment":null,"columns":[],"values":["x",1]}|}
          (Bad_table {index = 1; path = "Catalog.table.values"}));
    test_case "unknown field" `Quick (fun () ->
        check_text_error
          {|{"name":"a","comment":null,"columns":[],"values":null,"kind":"x"}|}
          (Bad_table {index = 1; path = "Catalog.table"});
        check_text_error
          (text
             [
               table "a"
                 [
                   (match column "x" "text" with
                   | `Assoc fields -> `Assoc (("extra", `Bool true) :: fields)
                   | other -> other);
                 ];
             ])
          (Bad_table {index = 1; path = "Catalog.column"}));
  ]

(* ---------- metadata ---------- *)

let metadata_tests =
  let open Alcotest in
  let entries = option (list (pair string (option string))) in
  let check_comment comment expected =
    check entries
      (Option.value ~default:"(none)" comment)
      expected
      (Metadata.of_comment comment)
  in
  [
    test_case "Metadata.of_comment" `Quick (fun () ->
        check_comment None (Some []);
        check_comment (Some "Free text, no metadata.") (Some []);
        check_comment (Some "Text.\nmetadata: kind=lookup, ocaml=A.B\tflag")
          (Some [("kind", Some "lookup"); ("ocaml", Some "A.B"); ("flag", None)]);
        check_comment (Some "metadata: a\nmetadata: b") None;
        check_comment (Some "  metadata: a") None;
        check_comment (Some "Metadata: a") None;
        check_comment (Some "metadata: a a") None;
        check_comment (Some "metadata: id=") None);
    test_case "malformed table metadata" `Quick (fun () ->
        check_error
          [table "a" ~comment:"metadata: id=" []]
          (Malformed_metadata {table = "a"; column = None}));
    test_case "malformed column metadata" `Quick (fun () ->
        check_error
          [table "a" [column "x" "text" ~comment:"Metadata: server_only"]]
          (Malformed_metadata {table = "a"; column = Some "x"}));
    test_case "unknown table key" `Quick (fun () ->
        check_error
          [table "a" ~comment:"metadata: idd=X" []]
          (Unknown_metadata {table = "a"; column = None; key = "idd=X"}));
    test_case "unknown kind" `Quick (fun () ->
        check_error
          [table "a" ~comment:"metadata: kind=enum" []]
          (Unknown_metadata {table = "a"; column = None; key = "kind=enum"}));
    test_case "ocaml= outside a lookup table" `Quick (fun () ->
        check_error
          [table "a" ~comment:"metadata: ocaml=X" []]
          (Unknown_metadata {table = "a"; column = None; key = "ocaml=X"}));
    test_case "id= on a lookup table" `Quick (fun () ->
        check_error
          [table "a" ~comment:"metadata: kind=lookup, ocaml=X, id=Y" []]
          (Unknown_metadata {table = "a"; column = None; key = "id=Y"}));
    test_case "unknown column key, or a flag with a value" `Quick (fun () ->
        check_error
          [table "a" [column "x" "text" ~comment:"metadata: serveronly"]]
          (Unknown_metadata {table = "a"; column = Some "x"; key = "serveronly"});
        check_error
          [table "a" [column "x" "text" ~comment:"metadata: no_reverse=1"]]
          (Unknown_metadata
             {table = "a"; column = Some "x"; key = "no_reverse=1"}));
    test_case "lookup tables' columns are not read" `Quick (fun () ->
        let tables =
          edited [("sources", column "x" "point" ~comment:"metadata: anything")]
        in
        check int "tables" 3 (List.length (model ~tables ()).tables));
  ]

(* ---------- types ---------- *)

let table_of (model : Model.t) name =
  match List.find_opt (fun (t : Model.table) -> t.name = name) model.tables with
  | Some t -> t
  | None -> Alcotest.failf "no table %s" name

let column_of (t : Model.table) name =
  match List.find_opt (fun (c : Model.column) -> c.name = name) t.columns with
  | Some c -> c
  | None -> Alcotest.failf "no column %s" name

let base_name : Model.base -> string = function
  | Str -> "Str"
  | Integer -> "Integer"
  | Numeric -> "Numeric"
  | Bool -> "Bool"
  | Timestamp_tz -> "Timestamp_tz"
  | Json -> "Json"
  | Custom c -> "Custom " ^ String.concat "." c.path

let value_name : Model.value -> string = function
  | Required -> "Required"
  | Defaulted -> "Defaulted"
  | Nullable -> "Nullable"

let check_column t name base value =
  let c = column_of t name in
  Alcotest.(check (pair string string))
    name (base, value)
    (base_name c.base, value_name c.value)

let types_tests =
  let open Alcotest in
  [
    test_case "plain types, nullability and defaults" `Quick (fun () ->
        let items = table_of (model ()) "items" in
        check_column items "name" "Str" "Required";
        check_column items "seen_at" "Timestamp_tz" "Nullable";
        check_column items "tags" "Json" "Required";
        check_column items "payload" "Json" "Required";
        check_column items "count" "Integer" "Defaulted";
        check_column items "score" "Numeric" "Nullable";
        check_column items "active" "Bool" "Required");
    test_case "ids and lookups" `Quick (fun () ->
        let m = model () in
        let items = table_of m "items" in
        check_column items "id" "Custom Domain.Ids.Item_id" "Required";
        check_column items "parent_id" "Custom Domain.Ids.Parent_id" "Nullable";
        check_column items "source" "Custom Domain.Thing.Source" "Required";
        check (list string) "codecs, lookups first"
          ["thing_source"; "ids_item_id"; "ids_note_id"; "ids_parent_id"]
          (List.map (fun (c : Model.codec) -> c.binding) m.codecs));
    test_case "without a module prefix, paths are as written" `Quick (fun () ->
        check_column
          (table_of (model ~module_prefix:[] ()) "items")
          "id" "Custom Ids.Item_id" "Required");
    test_case "lookup tables get no module; server_only columns are left out"
      `Quick (fun () ->
        let m = model () in
        check (list string) "tables"
          ["items"; "notes"; "parents"]
          (List.map (fun (t : Model.table) -> t.name) m.tables);
        check (list string) "parents" ["id"; "label"]
          (List.map
             (fun (c : Model.column) -> c.name)
             (table_of m "parents").columns));
    test_case "unsupported type" `Quick (fun () ->
        check_error
          [table "a" [column "p" "point"]]
          (Unsupported_type {table = "a"; column = "p"; sql_type = "point"}));
    test_case "uuid key without id=" `Quick (fun () ->
        check_error
          [table "a" [id]]
          (Uuid_without_id {table = "a"; column = "id"}));
    test_case "uuid that is neither a key nor a foreign key" `Quick (fun () ->
        check_error
          (edited [("parents", column "other" "uuid")])
          (Uuid_without_id {table = "parents"; column = "other"}));
    test_case "uuid foreign key to a table without id=" `Quick (fun () ->
        check_error
          [
            table "a" ~comment:"metadata: id=A"
              [id; column "b_id" "uuid" ~fk:"b"];
            table "b" [];
          ]
          (Uuid_without_id {table = "a"; column = "b_id"}));
    test_case "a text foreign key to a table without id= is text" `Quick
      (fun () ->
        let tables =
          [table "a" [column "b_key" "text" ~fk:"b"]; table "b" []]
        in
        check_column (table_of (model ~tables ()) "a") "b_key" "Str" "Nullable");
    test_case "lookup without ocaml=" `Quick (fun () ->
        check_error
          [
            table "a" [column "l" "text" ~fk:"l"];
            table "l" ~comment:"metadata: kind=lookup" [];
          ]
          (Lookup_without_ocaml {table = "a"; column = "l"; lookup = "l"}));
    test_case "a lookup without ocaml= nobody references" `Quick (fun () ->
        let tables =
          edited ~tables:[table "l" ~comment:"metadata: kind=lookup" []] []
        in
        check int "codecs" 4 (List.length (model ~tables ()).codecs));
    test_case "generated column" `Quick (fun () ->
        check_error
          [table "a" [column "g" "text" ~generated:true ~default:true]]
          (Generated_column {table = "a"; column = "g"}));
    test_case "a server_only generated column is left out" `Quick (fun () ->
        let tables =
          edited
            [
              ( "parents",
                column "g" "text" ~generated:true
                  ~comment:"metadata: server_only" );
            ]
        in
        check int "parents" 2
          (List.length (table_of (model ~tables ()) "parents").columns));
  ]

(* ---------- relationships ---------- *)

let relationship (r : Model.relationship) =
  let cardinality =
    match r.cardinality with
    | One -> "One"
    | Exactly_one -> "Exactly_one"
    | Many -> "Many"
  in
  Printf.sprintf "%s: %s %s" r.binding cardinality r.target

let relationships ?tables name =
  List.map relationship (table_of (model ?tables ()) name).relationships

let relationship_tests =
  let open Alcotest in
  [
    test_case "forward and reverse" `Quick (fun () ->
        check (list string) "items" ["parent: One parents"]
          (relationships "items");
        check (list string) "notes"
          ["item: Exactly_one items"]
          (relationships "notes");
        check (list string) "parents" ["items: Many items"]
          (relationships "parents"));
    test_case "without no_reverse, a Many" `Quick (fun () ->
        let tables =
          edited ~drop:[note_item]
            [("notes", column "item_id" "uuid" ~not_null:true ~fk:"items")]
        in
        check (list string) "items"
          ["parent: One parents"; "notes: Many notes"]
          (relationships ~tables "items"));
    test_case "a foreign key column without _id is named after its target"
      `Quick (fun () ->
        let tables = edited [("notes", column "about" "uuid" ~fk:"parents")] in
        check (list string) "notes"
          ["item: Exactly_one items"; "parents: One parents"]
          (relationships ~tables "notes"));
    test_case "two foreign keys between two tables" `Quick (fun () ->
        check_error
          (edited [("items", column "other_parent_id" "uuid" ~fk:"parents")])
          (Ambiguous_relationship
             {table = "items"; column = "other_parent_id"; other = "parents"}));
    test_case "foreign keys both ways between two tables" `Quick (fun () ->
        check_error
          (edited [("parents", column "item_id" "uuid" ~fk:"items")])
          (Ambiguous_relationship
             {table = "parents"; column = "item_id"; other = "items"}));
    test_case "a server_only foreign key still makes an embed ambiguous" `Quick
      (fun () ->
        check_error
          (edited
             [
               ( "parents",
                 column "item_id" "uuid" ~fk:"items"
                   ~comment:"metadata: server_only" );
             ])
          (Ambiguous_relationship
             {table = "parents"; column = "item_id"; other = "items"}));
    test_case "a server_only foreign key is not a relationship" `Quick
      (fun () ->
        let tables =
          edited
            [
              ( "notes",
                column "parent_id" "uuid" ~fk:"parents"
                  ~comment:"metadata: server_only" );
            ]
        in
        check (list string) "parents" ["items: Many items"]
          (relationships ~tables "parents"));
    test_case "unique foreign key" `Quick (fun () ->
        check_error
          (edited
             [("parents", column "note_id" "uuid" ~unique:true ~fk:"notes")])
          (Unique_foreign_key {table = "parents"; column = "note_id"}));
    test_case "unique foreign key with no_reverse" `Quick (fun () ->
        let tables =
          edited
            [
              ( "parents",
                column "note_id" "uuid" ~unique:true ~fk:"notes"
                  ~comment:"metadata: no_reverse" );
            ]
        in
        check (list string) "parents"
          ["note: One notes"; "items: Many items"]
          (relationships ~tables "parents"));
  ]

(* ---------- emission ---------- *)

let generate () =
  match Codegen.generate ~module_prefix:"Domain" (text fixture) with
  | Ok text -> text
  | Error e -> fail_with e

let print item = Format.asprintf "%a" Ppxlib.Pprintast.structure_item item

(* [code] parsed and printed back: the expected item in the printer's
   layout, so that items compare as ASTs, not as text. *)
let expected code =
  match Ppxlib.Parse.implementation (Lexing.from_string code) with
  | [item] -> print item
  | _ -> Alcotest.failf "expected one item in %S" code

let module_items name structure =
  List.find_map
    (fun (item : Ppxlib.structure_item) ->
      match item.pstr_desc with
      | Pstr_module
          {
            pmb_name = {txt = Some n; _};
            pmb_expr = {pmod_desc = Pmod_structure items; _};
            _;
          }
        when n = name ->
          Some items
      | _ -> None)
    structure
  |> function
  | Some items -> items
  | None -> Alcotest.failf "no module %s" name

let binding_name (item : Ppxlib.structure_item) =
  match item.pstr_desc with
  | Pstr_value (_, [{pvb_pat = {ppat_desc = Ppat_var {txt; _}; _}; _}])
  | Pstr_value
      ( _,
        [
          {
            pvb_pat =
              {
                ppat_desc =
                  Ppat_constraint ({ppat_desc = Ppat_var {txt; _}; _}, _);
                _;
              };
            _;
          };
        ] ) ->
      Some txt
  | _ -> None

let item name items =
  match List.find_opt (fun i -> binding_name i = Some name) items with
  | Some i -> print i
  | None -> Alcotest.failf "no binding %s" name

let emission_tests =
  let open Alcotest in
  let structure () = Emit.structure (model ()) in
  [
    test_case "header items" `Quick (fun () ->
        match structure () with
        | disable :: col :: items :: notes :: parents :: _ ->
            check string "ocamlformat disabled"
              (expected {|[@@@ocamlformat "disable"]|})
              (print disable);
            check string "Col"
              (expected "module Col = Postgrest.Col")
              (print col);
            check (list string) "phantom types"
              (List.map expected ["type items"; "type notes"; "type parents"])
              (List.map print [items; notes; parents])
        | _ -> Alcotest.fail "too few items");
    test_case "codecs" `Quick (fun () ->
        let s = structure () in
        check string "lookup codec"
          (expected
             {|let thing_source : Domain.Thing.Source.t Col.custom =
                 { name = "sources"; to_json = Domain.Thing.Source.to_json;
                   of_json = Domain.Thing.Source.of_json }|})
          (item "thing_source" s);
        check string "id codec"
          (expected
             {|let ids_item_id : Domain.Ids.Item_id.t Col.custom =
                 { name = "uuid"; to_json = Domain.Ids.Item_id.to_json;
                   of_json = Domain.Ids.Item_id.of_json }|})
          (item "ids_item_id" s));
    test_case "table module header" `Quick (fun () ->
        match module_items "Items" (structure ()) with
        | t :: table :: _ ->
            check string "type t" (expected "type t = items") (print t);
            check string "table"
              (expected {|let table : t Col.table = Col.table "items"|})
              (print table)
        | _ -> Alcotest.fail "too few items");
    test_case "columns" `Quick (fun () ->
        let items = module_items "Items" (structure ()) in
        check string "required"
          (expected
             {|let name : (t, string, [`Mandatory]) Col.t =
                 {ty = Not_Null Str; name = "name"}|})
          (item "name" items);
        check string "defaulted"
          (expected
             {|let count : (t, int, [`Optional]) Col.t =
                 {ty = Not_Null Integer; name = "count"}|})
          (item "count" items);
        check string "nullable"
          (expected
             {|let seen_at : (t, Ptime.t option, [`Optional]) Col.t =
                 {ty = Nullable TimestampTz; name = "seen_at"}|})
          (item "seen_at" items);
        check string "json"
          (expected
             {|let tags : (t, Yojson.Safe.t, [`Mandatory]) Col.t =
                 {ty = Not_Null JSON; name = "tags"}|})
          (item "tags" items);
        check string "float and bool"
          (expected
             {|let score : (t, float option, [`Optional]) Col.t =
                 {ty = Nullable Numeric; name = "score"}|}
          ^ expected
              {|let active : (t, bool, [`Mandatory]) Col.t =
                  {ty = Not_Null Bool; name = "active"}|}
          )
          (item "score" items ^ item "active" items);
        check string "custom"
          (expected
             {|let parent_id : (t, Domain.Ids.Parent_id.t option, [`Optional]) Col.t =
                 {ty = Nullable (Custom ids_parent_id); name = "parent_id"}|})
          (item "parent_id" items));
    test_case "relationships" `Quick (fun () ->
        let s = structure () in
        check string "One"
          (expected
             {|let parent : (t, parents, 'a, 'a option) Col.relationship =
                 {ty = One; name = "parents"}|})
          (item "parent" (module_items "Items" s));
        check string "Exactly_one"
          (expected
             {|let item : (t, items, 'a, 'a) Col.relationship =
                 {ty = Exactly_one; name = "items"}|})
          (item "item" (module_items "Notes" s));
        check string "Many"
          (expected
             {|let items : (t, items, 'a, 'a list) Col.relationship =
                 {ty = Many; name = "items"}|})
          (item "items" (module_items "Parents" s)));
    test_case "the printed file starts with the generated-file comment" `Quick
      (fun () ->
        check bool "header" true
          (String.starts_with ~prefix:"(* Generated by postgrest-codegen"
             (generate ())));
  ]

let () =
  Alcotest.run "codegen"
    [
      ("catalog", catalog_tests);
      ("metadata", metadata_tests);
      ("types", types_tests);
      ("relationships", relationship_tests);
      ("emission", emission_tests);
    ]

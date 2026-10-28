(* Integration tests for lib/postgrest.

   Two layers: unit checks on the pure pieces (filter rendering, row
   decoding, input encoding), and scenario replays: each entry of
   fixtures/scenarios.json is a real PostgREST exchange; the test makes the
   corresponding client call against Mock_http, checks the request the
   client produced against what was actually sent to PostgREST, feeds the
   recorded response back and checks the decoded value. *)

open Postgrest
open Tables
module C = Client.Make (Mock_http)

(* A plain PostgREST, as the fixtures were recorded: no gateway, no keys. *)
let base_url = Uri.of_string "http://localhost:3000"
let conn : Client.conn = {base_url; headers = (fun () -> [])}

(* ---------- testables ---------- *)

let yojson =
  Alcotest.testable
    (fun f j -> Format.pp_print_string f (Yojson.Safe.pretty_to_string j))
    Yojson.Safe.equal

let string_of_row_error = function
  | Row.Missing -> "Missing"
  | Row.NotAnObject -> "NotAnObject"
  | Row.WrongType -> "WrongType"
  | Row.UnexpectedNull -> "UnexpectedNull"
  | Row.DummyRow -> "DummyRow"

let string_of_error : C.error -> string = function
  | C.IOError e -> "IOError " ^ e
  | C.ErrorStatus n -> "ErrorStatus " ^ string_of_int n
  | C.BadResponse (Client.RowError (i, e)) ->
      Printf.sprintf "BadResponse (RowError (%d, %s))" i (string_of_row_error e)
  | C.BadResponse Client.ExpectedArray -> "BadResponse ExpectedArray"
  | C.BadResponse (Client.JsonError _) -> "BadResponse JsonError"

let error =
  Alcotest.testable
    (fun f e -> Format.pp_print_string f (string_of_error e))
    (fun a b -> String.equal (string_of_error a) (string_of_error b))

let row_error =
  Alcotest.testable
    (fun f e -> Format.pp_print_string f (string_of_row_error e))
    (fun a b -> String.equal (string_of_row_error a) (string_of_row_error b))

(* Compare decoded values through a JSON rendering: one testable for every
   record type, readable diffs. *)
let check_result name ~to_json expected actual =
  Alcotest.(check (result yojson error))
    name
    (Result.map to_json expected)
    (Result.map to_json actual)

let time_json t = `String (Ptime.to_rfc3339 ~tz_offset_s:0 t)
let opt f = function None -> `Null | Some v -> f v
let str s = `String s
let int n = `Int n
let float x = `Float x

let status_json = function
  | Active -> `String "active"
  | Archived -> `String "archived"

let plant_json (p : plant) =
  `Assoc
    [
      ("id", str p.id);
      ("name", str p.name);
      ("scientific_name", opt str p.scientific_name);
      ("status", status_json p.status);
      ("rating", opt int p.rating);
      ("height_cm", opt float p.height_cm);
      ("is_native", `Bool p.is_native);
      ("created_at", time_json p.created_at);
      ("tags", opt Fun.id p.tags);
    ]

let id_name_json (r : id_name) = `Assoc [("id", str r.id); ("name", str r.name)]

let obs_id_note_json (o : obs_id_note) =
  `Assoc [("id", str o.id); ("note", opt str o.note)]

let plant_with_obs_json (p : plant_with_obs) =
  let obs = List.sort (fun (a : obs_id_note) b -> compare a.id b.id) p.obs in
  `Assoc
    [
      ("id", str p.id);
      ("name", str p.name);
      ("obs", `List (List.map obs_id_note_json obs));
    ]

let obs_with_plant_json (o : obs_with_plant) =
  `Assoc
    [
      ("id", str o.id); ("note", opt str o.note); ("plant", id_name_json o.plant);
    ]

let list f l = `List (List.map f l)
let unit () = `Null

(* ---------- fixtures ---------- *)

type scenario = {
  name : string;
  meth : string;
  path_query : string;
  request_headers : (string * string) list;
  request_body : Yojson.Safe.t option;
  status : int;
  response_headers : (string * string) list;
  response_body : string; (* raw text handed back to the client *)
}

let scenarios : scenario list Lazy.t =
  lazy
    (let open Yojson.Safe.Util in
     let file = "fixtures/scenarios.json" in
     let json =
       try Yojson.Safe.from_file file
       with Sys_error _ ->
         Alcotest.failf
           "%s is missing: run scripts/record-postgrest-fixtures.sh" file
     in
     let headers j = to_assoc j |> List.map (fun (k, v) -> (k, to_string v)) in
     json
     |> to_list
     |> List.map (fun s ->
         {
           name = s |> member "name" |> to_string;
           meth = s |> member "method" |> to_string;
           path_query = s |> member "path_query" |> to_string;
           request_headers = s |> member "request_headers" |> headers;
           request_body =
             (match s |> member "request_body" with
             | `Null -> None
             | b -> Some b);
           status = s |> member "status" |> to_int;
           response_headers = s |> member "response_headers" |> headers;
           response_body =
             (match s |> member "response_body" with
             | `String raw -> raw
             | json -> Yojson.Safe.to_string json);
         }))

let scenario name =
  match
    List.find_opt (fun s -> String.equal s.name name) (Lazy.force scenarios)
  with
  | Some s -> s
  | None -> Alcotest.failf "no recorded scenario named %s" name

let response_json name =
  let s = scenario name in
  match String.equal s.response_body "" with
  | true -> Alcotest.failf "scenario %s has an empty body" name
  | false -> Yojson.Safe.from_string s.response_body

let prefer_set v =
  v
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> not (String.equal s ""))
  |> List.sort compare

(* Install the recorded response, run the client call, then check that the
   request the client produced is the one that was really sent. *)
let replay ?params name (call : unit -> 'a) : 'a =
  let s = scenario name in
  Mock_http.reset ();
  Mock_http.respond ~status:s.status ~headers:s.response_headers s.response_body;
  let result = call () in
  let req = Mock_http.last () in
  Alcotest.(check string) "method" s.meth (Mock_http.string_of_meth req.meth);
  (* PostgREST does not care about parameter order, and Uri.add_query_param'
     prepends, so compare the path and the set of parameters. *)
  let split target =
    match String.index_opt target '?' with
    | None -> (target, [])
    | Some i ->
        ( String.sub target 0 i,
          String.sub target (i + 1) (String.length target - i - 1)
          |> String.split_on_char '&'
          |> List.sort compare )
  in
  let expected_path, expected_params = split s.path_query in
  let expected_params =
    match params with None -> expected_params | Some p -> List.sort compare p
  in
  let actual_path, actual_params = split (Mock_http.decoded_target req.url) in
  Alcotest.(check string) "path" expected_path actual_path;
  Alcotest.(check (list string))
    "query parameters" expected_params actual_params;
  (match s.request_body with
  | None -> ()
  | Some expected ->
      let actual =
        match req.body with
        | None -> `Null
        | Some b -> Yojson.Safe.from_string b
      in
      Alcotest.(check yojson) "request body" expected actual);
  List.iter
    (fun (k, v) ->
      match (String.lowercase_ascii k, Mock_http.header k req) with
      | "prefer", Some actual ->
          Alcotest.(check (list string))
            "Prefer" (prefer_set v) (prefer_set actual)
      | _, Some actual -> Alcotest.(check string) ("header " ^ k) v actual
      | _, None -> Alcotest.failf "header %s was not sent" k)
    s.request_headers;
  result

(* ---------- filter rendering ---------- *)

let enc f = Filter.encode f

let check_param name expected f =
  Alcotest.(check (pair string string)) name expected (enc f)

let filter_tests =
  let open Filter in
  [
    ( "simple eq",
      `Quick,
      fun () ->
        check_param "eq" ("name", "eq.Arnica")
          (Eq (Plants.name, Value "Arnica")) );
    ( "simple value with comma is not quoted",
      `Quick,
      fun () ->
        check_param "eq"
          ("name", "eq.Plante, inconnue")
          (Eq (Plants.name, Value "Plante, inconnue")) );
    ( "nullable column value",
      `Quick,
      fun () ->
        check_param "gte" ("rating", "gte.4")
          (Gte (Plants.rating, Value (Some 4))) );
    ( "is null",
      `Quick,
      fun () ->
        check_param "is" ("rating", "is.null") (Is (Plants.rating, Value None))
    );
    ( "not is null",
      `Quick,
      fun () ->
        check_param "not.is" ("rating", "not.is.null")
          (Not (Is (Plants.rating, Value None))) );
    ( "numeric",
      `Quick,
      fun () ->
        check_param "gt" ("height_cm", "gt.30.5")
          (Gt (Plants.height_cm, Value (Some 30.5))) );
    ( "timestamp renders as UTC with Z",
      `Quick,
      fun () ->
        check_param "gt"
          ("taken_at", "gt.2026-06-15T00:00:00Z")
          (Gt (Observations.taken_at, Value (ptime "2026-06-15T00:00:00Z"))) );
    ( "enum",
      `Quick,
      fun () ->
        check_param "eq" ("status", "eq.archived")
          (Eq (Plants.status, Value Archived)) );
    ( "bool",
      `Quick,
      fun () ->
        check_param "eq" ("is_native", "eq.true")
          (Eq (Plants.is_native, Value true)) );
    ( "like keeps the star",
      `Quick,
      fun () ->
        check_param "like" ("name", "like.*ane*")
          (Like (Plants.name, Value "*ane*")) );
    ( "in list",
      `Quick,
      fun () ->
        check_param "in"
          ("status", "in.(active,archived)")
          (In (Plants.status, [Active; Archived])) );
    ( "in list quotes an element with a comma",
      `Quick,
      fun () ->
        check_param "in"
          ("name", "in.(\"Plante, inconnue\",Arnica)")
          (In (Plants.name, ["Plante, inconnue"; "Arnica"])) );
    ( "any",
      `Quick,
      fun () ->
        check_param "any"
          ("scientific_name", "eq(any).{Arnica montana,Gentiana lutea}")
          (Eq
             ( Plants.scientific_name,
               Any [Some "Arnica montana"; Some "Gentiana lutea"] )) );
    ( "any quotes an element with a comma",
      `Quick,
      fun () ->
        check_param "any"
          ("name", "eq(any).{\"Plante, inconnue\",Arnica}")
          (Eq (Plants.name, Any ["Plante, inconnue"; "Arnica"])) );
    ( "or tree",
      `Quick,
      fun () ->
        check_param "or"
          ("or", "(rating.gte.4,is_native.is.true)")
          (Or
             [
               Gte (Plants.rating, Value (Some 4));
               Is (Plants.is_native, Value true);
             ]) );
    ( "and tree with not and like",
      `Quick,
      fun () ->
        check_param "and"
          ("and", "(rating.not.is.null,name.like.A*)")
          (And
             [
               Not (Is (Plants.rating, Value None));
               Like (Plants.name, Value "A*");
             ]) );
    ( "not over a tree goes on the key",
      `Quick,
      fun () ->
        check_param "not.or"
          ("not.or", "(rating.gte.4,is_native.is.true)")
          (Not
             (Or
                [
                  Gte (Plants.rating, Value (Some 4));
                  Is (Plants.is_native, Value true);
                ])) );
    ( "nested tree",
      `Quick,
      fun () ->
        check_param "and"
          ("and", "(is_native.is.true,or(rating.is.null,rating.gte.5))")
          (And
             [
               Is (Plants.is_native, Value true);
               Or
                 [
                   Is (Plants.rating, Value None);
                   Gte (Plants.rating, Value (Some 5));
                 ];
             ]) );
    ( "tree value with a comma is quoted",
      `Quick,
      fun () ->
        check_param "or"
          ("or", "(name.eq.\"Plante, inconnue\",name.eq.Arnica)")
          (Or
             [
               Eq (Plants.name, Value "Plante, inconnue");
               Eq (Plants.name, Value "Arnica");
             ]) );
    ( "syntax module builds the same tree",
      `Quick,
      fun () ->
        let module S = Filter.Syntax in
        let f = S.(Plants.rating >= v (Some 4) || Plants.is_native == v true) in
        check_param "or" ("or", "(rating.gte.4,is_native.is.true)") f );
  ]

(* ---------- row decoding ---------- *)

let decode row s = Row.decode row (Yojson.Safe.from_string s)

let check_row name ~to_json expected row s =
  Alcotest.(check (result yojson row_error))
    name
    (Result.map to_json expected)
    (Result.map to_json (decode row s))

let row_tests =
  [
    ( "id and name",
      `Quick,
      fun () ->
        check_row "ok" ~to_json:id_name_json
          (Ok {id = "x"; name = "Arnica"})
          id_name_row {|{"id":"x","name":"Arnica","extra":1}|} );
    ( "missing field",
      `Quick,
      fun () ->
        check_row "missing" ~to_json:id_name_json (Error Row.Missing)
          id_name_row {|{"id":"x"}|} );
    ( "not an object",
      `Quick,
      fun () ->
        check_row "array" ~to_json:id_name_json (Error Row.NotAnObject)
          id_name_row {|[1]|} );
    ( "wrong type",
      `Quick,
      fun () ->
        check_row "wrong" ~to_json:id_name_json (Error Row.WrongType)
          id_name_row {|{"id":1,"name":"x"}|} );
    ( "null in a not-null column",
      `Quick,
      fun () ->
        check_row "null" ~to_json:id_name_json (Error Row.UnexpectedNull)
          id_name_row {|{"id":null,"name":"x"}|} );
    ( "nullable columns",
      `Quick,
      fun () ->
        check_row "nulls" ~to_json:obs_id_note_json
          (Ok {id = "o"; note = None})
          obs_id_note_row {|{"id":"o","note":null}|} );
    ( "numeric column given a JSON integer",
      `Quick,
      fun () ->
        let row = Row.col Plants.height_cm in
        check_row "int" ~to_json:(opt float) (Ok (Some 120.)) row
          {|{"height_cm":120}|} );
    ( "numeric column given a JSON float",
      `Quick,
      fun () ->
        let row = Row.col Plants.height_cm in
        check_row "float" ~to_json:(opt float) (Ok (Some 30.5)) row
          {|{"height_cm":30.5}|} );
    ( "enum column",
      `Quick,
      fun () ->
        let row = Row.col Plants.status in
        check_row "enum" ~to_json:status_json (Ok Archived) row
          {|{"status":"archived"}|} );
    ( "enum column with an unknown label",
      `Quick,
      fun () ->
        let row = Row.col Plants.status in
        check_row "enum" ~to_json:status_json (Error Row.WrongType) row
          {|{"status":"dormant"}|} );
    ( "timestamp with offset",
      `Quick,
      fun () ->
        let row = Row.col Plants.created_at in
        check_row "ts" ~to_json:time_json
          (Ok (ptime "2026-03-01T10:00:00Z"))
          row {|{"created_at":"2026-03-01T10:00:00+00:00"}|} );
    ( "timestamp with microseconds",
      `Quick,
      fun () ->
        let row = Row.col Plants.created_at in
        check_row "ts" ~to_json:time_json
          (Ok (ptime "2026-03-01T10:00:00.123456Z"))
          row {|{"created_at":"2026-03-01T10:00:00.123456+00:00"}|} );
    ( "json column",
      `Quick,
      fun () ->
        let row = Row.col Plants.tags in
        check_row "json" ~to_json:(opt Fun.id)
          (Ok (Some (`Assoc [("a", `Int 1)])))
          row {|{"tags":{"a":1}}|} );
    ( "embedded many",
      `Quick,
      fun () ->
        check_row "many" ~to_json:plant_with_obs_json
          (Ok
             {
               id = "p";
               name = "n";
               obs = [{id = "o1"; note = Some "x"}; {id = "o2"; note = None}];
             })
          plant_with_obs_row
          {|{"id":"p","name":"n","observations":[{"id":"o1","note":"x"},{"id":"o2","note":null}]}|}
    );
    ( "embedded many, empty",
      `Quick,
      fun () ->
        check_row "empty" ~to_json:plant_with_obs_json
          (Ok {id = "p"; name = "n"; obs = []})
          plant_with_obs_row {|{"id":"p","name":"n","observations":[]}|} );
    ( "embedded exactly one",
      `Quick,
      fun () ->
        check_row "one" ~to_json:obs_with_plant_json
          (Ok {id = "o"; note = None; plant = {id = "p"; name = "n"}})
          obs_with_plant_row
          {|{"id":"o","note":null,"plants":{"id":"p","name":"n"}}|} );
    ( "embedded exactly one given null",
      `Quick,
      fun () ->
        check_row "null" ~to_json:obs_with_plant_json (Error Row.UnexpectedNull)
          obs_with_plant_row {|{"id":"o","note":null,"plants":null}|} );
    ( "select list names every column once",
      `Quick,
      fun () ->
        Alcotest.(check string)
          "columns" "id,name,observations(id,note)"
          (Row.encode_column_list plant_with_obs_row) );
  ]

(* ---------- input encoding ---------- *)

let input_json i = Input.to_json i

let input_tests =
  let open Input in
  [
    ( "full row",
      `Quick,
      fun () ->
        let i : (plants, [`Full]) Input.t =
          {
            fields =
              [
                Mandatory (Plants.id, Value "x");
                Mandatory (Plants.name, Value "Nouvelle");
                Optional (Plants.is_native, Value false);
                Optional (Plants.rating, Unspecified);
              ];
          }
        in
        Alcotest.(check yojson)
          "json"
          (`Assoc
             [
               ("id", `String "x");
               ("name", `String "Nouvelle");
               ("is_native", `Bool false);
             ])
          (input_json i) );
    ( "partial row: unspecified dropped, Value None is null",
      `Quick,
      fun () ->
        let i : (plants, [`Partial]) Input.t =
          {
            fields =
              [
                Mandatory (Plants.id, Unspecified);
                Mandatory (Plants.name, Unspecified);
                Optional (Plants.rating, Value (Some 4));
                Optional (Plants.scientific_name, Value None);
              ];
          }
        in
        Alcotest.(check yojson)
          "json"
          (`Assoc [("rating", `Int 4); ("scientific_name", `Null)])
          (input_json i) );
    ( "enum and timestamp values",
      `Quick,
      fun () ->
        let i : (plants, [`Partial]) Input.t =
          {
            fields =
              [
                Optional (Plants.status, Value Archived);
                Optional
                  (Plants.created_at, Value (ptime "2026-03-01T10:00:00Z"));
              ];
          }
        in
        Alcotest.(check yojson)
          "json"
          (`Assoc
             [
               ("status", `String "archived");
               ("created_at", `String "2026-03-01T10:00:00Z");
             ])
          (input_json i) );
    ( "batch is a JSON array",
      `Quick,
      fun () ->
        let one : (plants, [`Full]) Input.t =
          {
            fields =
              [
                Mandatory (Plants.id, Value "x");
                Mandatory (Plants.name, Value "n");
              ];
          }
        in
        Alcotest.(check yojson)
          "json"
          (`List [`Assoc [("id", `String "x"); ("name", `String "n")]])
          (Yojson.Safe.from_string (Input.list_to_json_string [one])) );
  ]

(* ---------- headers and transport-level behaviour ---------- *)

let all = Filter.And []

let header_tests =
  [
    ( "Accept for a list is application/json",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "[]";
        let _ = C.select conn (Many id_name_row) all Plants.table in
        Alcotest.(check (option string))
          "Accept" (Some "application/json")
          (Mock_http.header "Accept" (Mock_http.last ())) );
    ( "Accept for a single object",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "{}";
        let _ = C.select conn (One id_name_row) all Plants.table in
        Alcotest.(check (option string))
          "Accept" (Some "application/vnd.pgrst.object+json")
          (Mock_http.header "Accept" (Mock_http.last ())) );
    ( "apikey and bearer token are sent",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "[]";
        let conn =
          Client.supabase_conn base_url ~apikey:"anon-key" ~token:(fun () ->
              Some "jwt")
        in
        let _ = C.select conn (Many id_name_row) all Plants.table in
        let req = Mock_http.last () in
        Alcotest.(check (option string))
          "apikey" (Some "anon-key")
          (Mock_http.header "apikey" req);
        Alcotest.(check (option string))
          "Authorization" (Some "Bearer jwt")
          (Mock_http.header "Authorization" req) );
    ( "no Authorization header without a token, apikey still sent",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "[]";
        let conn =
          Client.supabase_conn base_url ~apikey:"anon-key" ~token:(fun () ->
              None)
        in
        let _ = C.select conn (Many id_name_row) all Plants.table in
        Alcotest.(check (option string))
          "apikey" (Some "anon-key")
          (Mock_http.header "apikey" (Mock_http.last ()));
        Alcotest.(check (option string))
          "Authorization" None
          (Mock_http.header "Authorization" (Mock_http.last ())) );
    ( "mutations send Content-Type: application/json",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond ~status:201 "";
        let input : (plants, [`Full]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.id, Value "x");
                  Mandatory (Plants.name, Value "n");
                ];
          }
        in
        let _ = C.insert conn ~returning:Zero input Plants.table in
        Alcotest.(check (option string))
          "Content-Type" (Some "application/json")
          (Mock_http.header "Content-Type" (Mock_http.last ())) );
    ( "query values are percent-encoded in the URL",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "[]";
        let where =
          Filter.(Eq (Plants.scientific_name, Value (Some "Arnica montana")))
        in
        let _ = C.select conn (Many id_name_row) where Plants.table in
        let url = Uri.to_string (Mock_http.last ()).url in
        Alcotest.(check bool) "no raw space" false (String.contains url ' ');
        Alcotest.(check bool)
          "space encoded" true
          (Option.is_some (String.index_from_opt url 0 '%')) );
    ( "base url path prefix is kept",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "[]";
        let conn =
          {conn with base_url = Uri.of_string "https://xyz.supabase.co/rest/v1"}
        in
        let _ = C.select conn (Many id_name_row) all Plants.table in
        Alcotest.(check string)
          "path" "/rest/v1/plants"
          (Uri.path (Mock_http.last ()).url) );
    ( "transport error is IOError",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.fail_with "offline";
        check_result "io" ~to_json:(list id_name_json)
          (Error (C.IOError "offline"))
          (C.select conn (Many id_name_row) all Plants.table) );
    ( "invalid JSON body is a BadResponse",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond "<html>not json</html>";
        check_result "json" ~to_json:(list id_name_json)
          (Error (C.BadResponse (Client.JsonError "")))
          (C.select conn (Many id_name_row) all Plants.table) );
    ( "object where an array was expected",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond {|{"id":"x","name":"n"}|};
        check_result "shape" ~to_json:(list id_name_json)
          (Error (C.BadResponse Client.ExpectedArray))
          (C.select conn (Many id_name_row) all Plants.table) );
    ( "row error carries the index of the bad row",
      `Quick,
      fun () ->
        Mock_http.reset ();
        Mock_http.respond {|[{"id":"a","name":"n"},{"id":"b"}]|};
        check_result "index" ~to_json:(list id_name_json)
          (Error (C.BadResponse (Client.RowError (1, Row.Missing))))
          (C.select conn (Many id_name_row) all Plants.table) );
  ]

(* ---------- scenario replays ---------- *)

let where_id n = Filter.(Eq (Plants.id, Value (uuid n)))
let ok x = Ok x

let select_scenarios =
  [
    ( "select_all_plants",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list plant_json)
          (ok [arnica; gentiane; inconnue])
          (replay "select_all_plants" (fun () ->
               C.select conn ~order:(Row.col Plants.name) (Many plant_row) all
                 Plants.table)) );
    ( "select_one_plant",
      `Quick,
      fun () ->
        check_result "row" ~to_json:id_name_json
          (ok {id = uuid 1; name = "Arnica"})
          (replay "select_one_plant" (fun () ->
               C.select conn (One id_name_row) (where_id 1) Plants.table)) );
    ( "select_one_missing",
      `Quick,
      fun () ->
        check_result "406" ~to_json:id_name_json (Error (C.ErrorStatus 406))
          (replay "select_one_missing" (fun () ->
               C.select conn (One id_name_row) (where_id 99) Plants.table)) );
    ( "select_eq_with_space",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 1; name = "Arnica"}])
          (replay "select_eq_with_space" (fun () ->
               C.select conn (Many id_name_row)
                 Filter.(
                   Eq (Plants.scientific_name, Value (Some "Arnica montana")))
                 Plants.table)) );
    ( "select_eq_with_comma",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 3; name = "Plante, inconnue"}])
          (replay "select_eq_with_comma" (fun () ->
               C.select conn (Many id_name_row)
                 Filter.(Eq (Plants.name, Value "Plante, inconnue"))
                 Plants.table)) );
    ( "select_in_enum",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ status = Row.col Plants.status in
          (id, status)
        in
        let to_json (id, s) =
          `Assoc [("id", str id); ("status", status_json s)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 1, Active); (uuid 2, Active); (uuid 3, Archived)])
          (replay "select_in_enum" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many row)
                 Filter.(In (Plants.status, [Active; Archived]))
                 Plants.table)) );
    ( "select_in_with_quoted_comma",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 3; name = "Plante, inconnue"};
             ])
          (replay "select_in_with_quoted_comma" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(In (Plants.name, ["Plante, inconnue"; "Arnica"]))
                 Plants.table)) );
    ( "select_or",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 2; name = "Gentiane jaune"};
             ])
          (replay "select_or" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(
                   Or
                     [
                       Gte (Plants.rating, Value (Some 4));
                       Is (Plants.is_native, Value true);
                     ])
                 Plants.table)) );
    ( "select_and_in_tree",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 1; name = "Arnica"}])
          (replay "select_and_in_tree"
             ~params:["rating=not.is.null"; "name=like.A*"; "select=id,name"]
             (fun () ->
               C.select conn (Many id_name_row)
                 Filter.(
                   And
                     [
                       Not (Is (Plants.rating, Value None));
                       Like (Plants.name, Value "A*");
                     ])
                 Plants.table)) );
    ( "select_not_or",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 3; name = "Plante, inconnue"}])
          (replay "select_not_or" (fun () ->
               C.select conn (Many id_name_row)
                 Filter.(
                   Not
                     (Or
                        [
                          Gte (Plants.rating, Value (Some 4));
                          Is (Plants.is_native, Value true);
                        ]))
                 Plants.table)) );
    ( "select_nested_tree",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 2; name = "Gentiane jaune"};
             ])
          (replay "select_nested_tree"
             ~params:
               [
                 "is_native=is.true";
                 "or=(rating.is.null,rating.gte.5)";
                 "order=id";
                 "select=id,name";
               ] (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(
                   And
                     [
                       Is (Plants.is_native, Value true);
                       Or
                         [
                           Is (Plants.rating, Value None);
                           Gte (Plants.rating, Value (Some 5));
                         ];
                     ])
                 Plants.table)) );
    ( "select_tree_value_with_comma",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 3; name = "Plante, inconnue"};
             ])
          (replay "select_tree_value_with_comma" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(
                   Or
                     [
                       Eq (Plants.name, Value "Plante, inconnue");
                       Eq (Plants.name, Value "Arnica");
                     ])
                 Plants.table)) );
    ( "select_any",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 2; name = "Gentiane jaune"};
             ])
          (replay "select_any" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(
                   Eq
                     ( Plants.scientific_name,
                       Any [Some "Arnica montana"; Some "Gentiana lutea"] ))
                 Plants.table)) );
    ( "select_any_quoted",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok
             [
               {id = uuid 1; name = "Arnica"};
               {id = uuid 3; name = "Plante, inconnue"};
             ])
          (replay "select_any_quoted" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(Eq (Plants.name, Any ["Plante, inconnue"; "Arnica"]))
                 Plants.table)) );
    ( "select_is_null",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ rating = Row.col Plants.rating in
          (id, rating)
        in
        let to_json (id, r) = `Assoc [("id", str id); ("rating", opt int r)] in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 2, None)])
          (replay "select_is_null" (fun () ->
               C.select conn (Many row)
                 Filter.(Is (Plants.rating, Value None))
                 Plants.table)) );
    ( "select_not_is_null",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ rating = Row.col Plants.rating in
          (id, rating)
        in
        let to_json (id, r) = `Assoc [("id", str id); ("rating", opt int r)] in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 1, Some 5); (uuid 3, Some 2)])
          (replay "select_not_is_null" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many row)
                 Filter.(Not (Is (Plants.rating, Value None)))
                 Plants.table)) );
    ( "select_numeric_gt",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ h = Row.col Plants.height_cm in
          (id, h)
        in
        let to_json (id, h) =
          `Assoc [("id", str id); ("height_cm", opt float h)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 2, Some 120.)])
          (replay "select_numeric_gt" (fun () ->
               C.select conn (Many row)
                 Filter.(Gt (Plants.height_cm, Value (Some 30.5)))
                 Plants.table)) );
    ( "select_numeric_gte",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ h = Row.col Plants.height_cm in
          (id, h)
        in
        let to_json (id, h) =
          `Assoc [("id", str id); ("height_cm", opt float h)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 1, Some 30.5); (uuid 2, Some 120.)])
          (replay "select_numeric_gte" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many row)
                 Filter.(Gte (Plants.height_cm, Value (Some 30.5)))
                 Plants.table)) );
    ( "select_timestamp_gt",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Observations.id
          and+ t = Row.col Observations.taken_at in
          (id, t)
        in
        let to_json (id, t) =
          `Assoc [("id", str id); ("taken_at", time_json t)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok
             [
               (obs_uuid 2, ptime "2026-07-03T09:10:00Z");
               (obs_uuid 3, ptime "2026-06-20T11:00:00Z");
             ])
          (replay "select_timestamp_gt" (fun () ->
               C.select conn ~order:(Row.col Observations.id) (Many row)
                 Filter.(
                   Gt
                     ( Observations.taken_at,
                       Value (ptime "2026-06-15T00:00:00Z") ))
                 Observations.table)) );
    ( "select_enum_eq",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ s = Row.col Plants.status in
          (id, s)
        in
        let to_json (id, s) =
          `Assoc [("id", str id); ("status", status_json s)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 3, Archived)])
          (replay "select_enum_eq" (fun () ->
               C.select conn (Many row)
                 Filter.(Eq (Plants.status, Value Archived))
                 Plants.table)) );
    ( "select_bool_eq",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ b = Row.col Plants.is_native in
          (id, b)
        in
        let to_json (id, b) = `Assoc [("id", str id); ("is_native", `Bool b)] in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 1, true); (uuid 2, true)])
          (replay "select_bool_eq" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many row)
                 Filter.(Eq (Plants.is_native, Value true))
                 Plants.table)) );
    ( "select_like",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 2; name = "Gentiane jaune"}])
          (replay "select_like" (fun () ->
               C.select conn ~order:(Row.col Plants.id) (Many id_name_row)
                 Filter.(Like (Plants.name, Value "*ane*"))
                 Plants.table)) );
    ( "select_ilike",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list id_name_json)
          (ok [{id = uuid 1; name = "Arnica"}])
          (replay "select_ilike" (fun () ->
               C.select conn (Many id_name_row)
                 Filter.(ILike (Plants.name, Value "*ARNICA*"))
                 Plants.table)) );
    ( "select_jsonb",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id and+ tags = Row.col Plants.tags in
          (id, tags)
        in
        let to_json (id, t) = `Assoc [("id", str id); ("tags", opt Fun.id t)] in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 1, arnica.tags)])
          (replay "select_jsonb" (fun () ->
               C.select conn (Many row) (where_id 1) Plants.table)) );
    ( "select_embed_many",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list plant_with_obs_json)
          (ok
             [
               {
                 id = uuid 1;
                 name = "Arnica";
                 obs =
                   [
                     {id = obs_uuid 1; note = Some "En fleur"};
                     {id = obs_uuid 2; note = None};
                   ];
               };
             ])
          (replay "select_embed_many" (fun () ->
               C.select conn (Many plant_with_obs_row) (where_id 1) Plants.table))
    );
    ( "select_embed_many (one observation)",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id
          and+ obs = Row.rel Plants.observations (Row.col Observations.id) in
          (id, obs)
        in
        let to_json (id, obs) =
          `Assoc [("id", str id); ("observations", list str obs)]
        in
        check_result "rows" ~to_json:(list to_json)
          (ok [(uuid 2, [obs_uuid 3])])
          (replay "select_embed_many_empty" (fun () ->
               C.select conn (Many row) (where_id 2) Plants.table)) );
    ( "select_embed_to_one",
      `Quick,
      fun () ->
        check_result "rows" ~to_json:(list obs_with_plant_json)
          (ok
             [
               {
                 id = obs_uuid 1;
                 note = Some "En fleur";
                 plant = {id = uuid 1; name = "Arnica"};
               };
             ])
          (replay "select_embed_to_one" (fun () ->
               C.select conn (Many obs_with_plant_row)
                 Filter.(Eq (Observations.id, Value (obs_uuid 1)))
                 Observations.table)) );
    ( "select_bad_column",
      `Quick,
      fun () ->
        let row = Row.col {Col.ty = Not_Null Str; name = "nope"} in
        check_result "400" ~to_json:(list str) (Error (C.ErrorStatus 400))
          (replay "select_bad_column" (fun () ->
               C.select conn (Many row) all Plants.table)) );
    ( "select_bad_table",
      `Quick,
      fun () ->
        let table : plants Col.table = Col.table "nothing" in
        check_result "404" ~to_json:(list str) (Error (C.ErrorStatus 404))
          (replay "select_bad_table" (fun () ->
               C.select conn (Many (Row.col Plants.id)) all table)) );
  ]

let new_plant : (plants, [`Full]) Input.t =
  {
    fields =
      Input.
        [
          Mandatory (Plants.id, Value (uuid 4));
          Mandatory (Plants.name, Value "Nouvelle");
          Optional (Plants.is_native, Value false);
          Optional (Plants.scientific_name, Unspecified);
          Optional (Plants.rating, Unspecified);
        ];
  }

let other_plant : (plants, [`Full]) Input.t =
  {
    fields =
      Input.
        [
          Mandatory (Plants.id, Value (uuid 5));
          Mandatory (Plants.name, Value "Autre");
          Optional (Plants.is_native, Value true);
          Optional (Plants.rating, Value (Some 3));
        ];
  }

(* `returning` is typed ('table, unit, [`Mutation]) card in client.mli, so
   only Zero is usable; the representation-returning scenarios are checked
   at the decoding level only. *)
let decode_body name row =
  match response_json name with
  | `List l ->
      List.map (Row.decode row) l
      |> List.map (Result.map_error string_of_row_error)
  | j -> [Row.decode row j |> Result.map_error string_of_row_error]

let mutation_scenarios =
  [
    ( "insert_minimal",
      `Quick,
      fun () ->
        check_result "unit" ~to_json:unit (ok ())
          (replay "insert_minimal" (fun () ->
               C.insert conn ~returning:Zero new_plant Plants.table)) );
    ( "insert_duplicate_error",
      `Quick,
      fun () ->
        let dup : (plants, [`Full]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.id, Value (uuid 1));
                  Mandatory (Plants.name, Value "Doublon");
                  Optional (Plants.is_native, Value false);
                ];
          }
        in
        check_result "409" ~to_json:unit (Error (C.ErrorStatus 409))
          (replay "insert_duplicate_error" (fun () ->
               C.insert conn ~returning:Zero dup Plants.table)) );
    ( "insert_batch_minimal",
      `Quick,
      fun () ->
        let first : (plants, [`Full]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.id, Value (uuid 4));
                  Mandatory (Plants.name, Value "Nouvelle");
                  Optional (Plants.is_native, Value false);
                  Optional (Plants.rating, Value None);
                ];
          }
        in
        check_result "unit" ~to_json:unit (ok ())
          (replay "insert_batch_minimal" (fun () ->
               C.insert_batch conn ~returning:Zero [first; other_plant]
                 Plants.table)) );
    ( "insert_batch_mismatched_keys is a 400 from PostgREST",
      `Quick,
      fun () ->
        check_result "400" ~to_json:unit (Error (C.ErrorStatus 400))
          (replay "insert_batch_mismatched_keys" (fun () ->
               C.insert_batch conn ~returning:Zero [new_plant; other_plant]
                 Plants.table)) );
    ( "upsert_merge_minimal",
      `Quick,
      fun () ->
        let row : (plants, [`Full]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.id, Value (uuid 1));
                  Mandatory (Plants.name, Value "Arnica (maj)");
                  Optional (Plants.is_native, Value true);
                  Optional (Plants.rating, Value (Some 4));
                ];
          }
        in
        check_result "unit" ~to_json:unit (ok ())
          (replay "upsert_merge_minimal" (fun () ->
               C.insert conn ~returning:Zero ~on_conflict:`Upsert row
                 Plants.table)) );
    ( "upsert_ignore_minimal",
      `Quick,
      fun () ->
        let row : (plants, [`Full]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.id, Value (uuid 1));
                  Mandatory (Plants.name, Value "Ignored");
                  Optional (Plants.is_native, Value true);
                ];
          }
        in
        check_result "unit" ~to_json:unit (ok ())
          (replay "upsert_ignore_minimal" (fun () ->
               C.insert conn ~returning:Zero ~on_conflict:`Ignore row
                 Plants.table)) );
    ( "insert_returning_object (decode only)",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ p = id_name_row
          and+ status = Row.col Plants.status
          and+ created = Row.col Plants.created_at in
          (p, status, created)
        in
        let to_json (p, s, c) =
          `Assoc
            [
              ("p", id_name_json p);
              ("status", status_json s);
              ("created_at", time_json c);
            ]
        in
        Alcotest.(check (list (result yojson string)))
          "decoded"
          [
            Ok
              (to_json
                 ( {id = uuid 4; name = "Nouvelle"},
                   Active,
                   ptime "2026-01-01T00:00:00Z" ));
          ]
          (decode_body "insert_returning_object" row
          |> List.map (Result.map to_json)) );
    ( "insert_batch_returning_array (decode only)",
      `Quick,
      fun () ->
        Alcotest.(check (list (result yojson string)))
          "decoded"
          [
            Ok (id_name_json {id = uuid 4; name = "Nouvelle"});
            Ok (id_name_json {id = uuid 5; name = "Autre"});
          ]
          (decode_body "insert_batch_returning_array" id_name_row
          |> List.map (Result.map id_name_json)) );
    ( "upsert_merge (decode only)",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ p = id_name_row and+ rating = Row.col Plants.rating in
          (p, rating)
        in
        let to_json (p, r) =
          `Assoc [("p", id_name_json p); ("rating", opt int r)]
        in
        Alcotest.(check (list (result yojson string)))
          "decoded"
          [Ok (to_json ({id = uuid 1; name = "Arnica (maj)"}, Some 4))]
          (decode_body "upsert_merge" row |> List.map (Result.map to_json)) );
    ( "update_partial_minimal",
      `Quick,
      fun () ->
        let patch : (plants, [`Partial]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.name, Unspecified);
                  Optional (Plants.rating, Value (Some 4));
                ];
          }
        in
        check_result "unit" ~to_json:unit (ok ())
          (replay "update_partial_minimal" (fun () ->
               C.update conn ~returning:Zero patch (where_id 2) Plants.table))
    );
    ( "update_returning_object (decode only)",
      `Quick,
      fun () ->
        let row =
          let open Row.Syntax in
          let+ id = Row.col Plants.id
          and+ rating = Row.col Plants.rating
          and+ sci = Row.col Plants.scientific_name in
          (id, rating, sci)
        in
        let to_json (id, r, s) =
          `Assoc
            [
              ("id", str id);
              ("rating", opt int r);
              ("scientific_name", opt str s);
            ]
        in
        Alcotest.(check (list (result yojson string)))
          "decoded"
          [Ok (to_json (uuid 2, Some 4, None))]
          (decode_body "update_returning_object" row
          |> List.map (Result.map to_json)) );
    ( "update_empty_body",
      `Quick,
      fun () ->
        let s = scenario "update_empty_body" in
        let patch : (plants, [`Partial]) Input.t =
          {
            fields =
              Input.
                [
                  Mandatory (Plants.name, Unspecified);
                  Optional (Plants.rating, Unspecified);
                ];
          }
        in
        let expected =
          match s.status < 400 with
          | true -> ok ()
          | false -> Error (C.ErrorStatus s.status)
        in
        check_result "unit" ~to_json:unit expected
          (replay "update_empty_body" (fun () ->
               C.update conn ~returning:Zero patch (where_id 2) Plants.table))
    );
    ( "delete_minimal",
      `Quick,
      fun () ->
        check_result "unit" ~to_json:unit (ok ())
          (replay "delete_minimal" (fun () ->
               C.delete conn ~returning:Zero
                 Filter.(Eq (Observations.id, Value (obs_uuid 4)))
                 Observations.table)) );
    ( "delete_fk_violation",
      `Quick,
      fun () ->
        check_result "409" ~to_json:unit (Error (C.ErrorStatus 409))
          (replay "delete_fk_violation" (fun () ->
               C.delete conn ~returning:Zero (where_id 1) Plants.table)) );
    ( "delete_returning_array (decode only)",
      `Quick,
      fun () ->
        Alcotest.(check (list (result yojson string)))
          "decoded"
          [
            Ok (obs_id_note_json {id = obs_uuid 1; note = Some "En fleur"});
            Ok (obs_id_note_json {id = obs_uuid 2; note = None});
          ]
          (decode_body "delete_returning_array" obs_id_note_row
          |> List.map (Result.map obs_id_note_json)) );
  ]

let () =
  Alcotest.run "postgrest"
    [
      ("filters", filter_tests);
      ("rows", row_tests);
      ("inputs", input_tests);
      ("client", header_tests);
      ("scenarios: select", select_scenarios);
      ("scenarios: mutations", mutation_scenarios);
    ]

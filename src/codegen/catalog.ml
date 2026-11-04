type column = {
  name : string;
  sql_type : string; [@key "type"]
  not_null : bool;
  has_default : bool;
  is_generated : bool;
  is_primary_key : bool;
  is_unique : bool;
  comment : string option;
  foreign_key : string option; [@key "fk_table"]
}
[@@deriving of_yojson]

type table = {
  name : string;
  comment : string option;
  columns : column list;
  values : string list option;
}
[@@deriving of_yojson]

type t = table list

let of_string text =
  let rec go index acc values =
    match values () with
    | Seq.Nil -> Ok (List.rev acc)
    | Seq.Cons (json, rest) -> (
        match table_of_yojson json with
        | Ok table -> go (index + 1) (table :: acc) rest
        | Error path -> Error (Error.Bad_table {index; path}))
    | exception Yojson.Json_error message -> Error (Error.Bad_json message)
  in
  go 1 [] (Yojson.Safe.seq_from_string text)

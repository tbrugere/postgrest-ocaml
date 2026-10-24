open Col.BaseTypes

type (_, _) value =
  | Value : 'a -> ('a, [< `Full | `Partial ]) value
  | Unspecified : ('a, [ `Partial ]) value

type (_, _) field =
  | Mandatory :
      ('table, 'a, [ `Mandatory ]) Col.t * ('a, 'mode) value
      -> ('table, 'mode) field
  | Optional :
      ('table, 'a, [ `Optional ]) Col.t * ('a, 'mode) value
      -> ('table, [< `Full | `Partial ]) field

type ('table, 'mode) t = { fields : ('table, 'mode) field list }

let to_json (input : ('table, 'mode) t) : Yojson.Safe.t =
  let single_to_json : type mode.
      ('table, mode) field -> (string * Yojson.Safe.t) option = function
    | Mandatory ({ name; ty }, Value a) -> Some (name, Col.to_json ty a)
    | Optional ({ name; ty }, Value a) -> Some (name, Col.to_json ty a)
    | Mandatory (_, Unspecified) | Optional (_, Unspecified) -> None
  in
  input.fields |> List.filter_map single_to_json |> fun l -> `Assoc l

let to_json_string (input : ('table, 'mode) t) : string =
  input |> to_json |> Yojson.Safe.to_string

let list_to_json_string (inputs : ('table, 'mode) t list) : string =
  inputs |> List.map to_json |> (fun l -> `List l) |> Yojson.Safe.to_string

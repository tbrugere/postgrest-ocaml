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

val to_json : ('table, 'mode) t -> Yojson.Safe.t
val to_json_string : ('table, 'mode) t -> string
val list_to_json_string : ('table, 'mode) t list -> string

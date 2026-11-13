type json = Yojson.Safe.t

type 'a custom = {
  name : string;
  to_json : 'a -> Yojson.Safe.t;
  of_json : Yojson.Safe.t -> 'a option;
}

type _ base =
  | Integer : int base
  | Numeric : float base
  | Bool : bool base
  | Str : string base
  | TimestampTz : Ptime.t base
  | JSON : Yojson.Safe.t base
  | Custom : 'a custom -> 'a base

type _ ty = Nullable : 'a base -> 'a option ty | Not_Null : 'a base -> 'a ty
type 'table table = {name : string}
type optional = [`Optional | `Mandatory]
type ('table, 'a, 'optional) t = {ty : 'a ty; name : string}

type (_, _) relationship_type =
  | One : ('a, 'a option) relationship_type
  | Many : ('a, 'a list) relationship_type
  | Exactly_one : ('a, 'a) relationship_type

type ('sourcetable, 'desttable, 'a, 'out) relationship = {
  ty : ('a, 'out) relationship_type;
  name : string;
}

let table name : 'table table = {name}
let name_of_table (table : 'table table) = table.name

let string_of_custom : 'a custom -> 'a -> string =
 fun {to_json; _} a ->
  match to_json a with `String s -> s | json -> Yojson.Safe.to_string json

let encode_base : type a. a base -> a -> string =
 fun base value ->
  match base with
  | Integer -> string_of_int value
  | Numeric -> string_of_float value
  | Bool -> string_of_bool value
  | Str -> value
  | TimestampTz -> Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:6 value
  | JSON -> Yojson.Safe.to_string value
  | Custom custom -> string_of_custom custom value

let encode_value : type a. a ty -> a -> string =
 fun ty value ->
  match (ty, value) with
  | Nullable _, None -> "null"
  | Nullable base, Some v -> encode_base base v
  | Not_Null base, v -> encode_base base v

let base_to_json : type a. a base -> a -> Yojson.Safe.t =
 fun base value ->
  match base with
  | Integer -> `Int value
  | Numeric -> `Float value
  | Bool -> `Bool value
  | Str -> `String value
  | TimestampTz -> `String (Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:6 value)
  | JSON -> value
  | Custom custom -> custom.to_json value

let to_json : type a. a ty -> a -> Yojson.Safe.t =
 fun ty value ->
  match (ty, value) with
  | Nullable _, None -> `Null
  | Nullable base, Some v -> base_to_json base v
  | Not_Null base, v -> base_to_json base v

let decode_base : type a. a base -> json -> (a option, [> `WrongType]) result =
 fun base value ->
  match (base, value) with
  | _, `Null -> Ok None
  | Integer, `Int a -> Ok (Some a)
  | Integer, `Intlit a -> Ok (Some (int_of_string a))
  | Numeric, `Float a -> Ok (Some a)
  | Numeric, `Int a -> Ok (Some (float_of_int a))
  | Numeric, `Intlit a -> Ok (Some (float_of_string a))
  | Bool, `Bool a -> Ok (Some a)
  | Str, `String a -> Ok (Some a)
  | TimestampTz, `String a ->
      begin match Ptime.of_rfc3339 a with
      | Ok (t, _, _) -> Ok (Some t)
      | _ -> Error `WrongType
      end
  | JSON, a -> Ok (Some a)
  | Custom custom, json ->
      begin match custom.of_json json with
      | Some e -> Ok (Some e)
      | None -> Error `WrongType
      end
  | _ -> Error `WrongType

let decode_type : type a.
    a ty -> json -> (a, [> `WrongType | `UnexpectedNull]) result =
 fun ty value ->
  match ty with
  | Nullable base -> decode_base base value
  | Not_Null base ->
      let open Result.Syntax in
      let* decoded = decode_base base value in
      Option.to_result ~none:`UnexpectedNull decoded

module BaseTypes = struct
  type nonrec 'a ty = 'a ty
  type nonrec 'a base = 'a base

  type nonrec ('sourcetable, 'desttable, 'a, 'out) relationship =
    ('sourcetable, 'desttable, 'a, 'out) relationship

  type nonrec 'table table = 'table table
end

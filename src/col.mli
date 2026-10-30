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
type 'table table
type optional = [`Mandatory | `Optional]
type ('table, 'a, 'optional) t = {ty : 'a ty; name : string}

type (_, _) relationship_type =
  | One : ('a, 'a option) relationship_type
  | Many : ('a, 'a list) relationship_type
  | Exactly_one : ('a, 'a) relationship_type

type ('sourcetable, 'desttable, 'a, 'out) relationship = {
  ty : ('a, 'out) relationship_type;
  name : string;
}

val table : string -> 'table table
val name_of_table : 'table table -> string
val encode_base : 'a base -> 'a -> string
val encode_value : 'a ty -> 'a -> string
val base_to_json : 'a base -> 'a -> Yojson.Safe.t
val to_json : 'a ty -> 'a -> Yojson.Safe.t

module BaseTypes : sig
  type nonrec 'a ty = 'a ty
  type nonrec 'a base = 'a base

  type nonrec ('sourcetable, 'desttable, 'a, 'out) relationship =
    ('sourcetable, 'desttable, 'a, 'out) relationship

  type nonrec 'table table = 'table table
end

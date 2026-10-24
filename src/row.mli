type json = Yojson.Safe.t
type error = Missing | NotAnObject | WrongType | UnexpectedNull | DummyRow
type ('table, 'a) t

val dummy_row : ('table, 'a) t
val decode : ('table, 'a) t -> json -> ('a, error) result
val get_row : string -> json -> (json, error) result
val col : ('table, 'a, 'o) Col.t -> ('table, 'a) t

val rel :
  ('sourcetable, 'desttable, 'a, 'out) Col.BaseTypes.relationship ->
  ('desttable, 'a) t ->
  ('sourcetable, 'out) t

val map : ('a -> 'b) -> ('table, 'a) t -> ('table, 'b) t
val pair : ('table, 'a) t -> ('table, 'b) t -> ('table, 'a * 'b) t
val encode_column_list : ('table, 'a) t -> string

module Syntax : sig
  val ( let+ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( and+ ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

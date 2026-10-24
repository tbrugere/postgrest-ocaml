type 'a rhs = Value of 'a | Any of 'a list | All of 'a list

type 'table t =
  | Eq : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Gt : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Gte : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Lt : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Lte : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Neq : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Like : ('table, string, 'o) Col.t * string rhs -> 'table t
  | ILike : ('table, string, 'o) Col.t * string rhs -> 'table t
  | Match : ('table, string, 'o) Col.t * string rhs -> 'table t
  | IMatch : ('table, string, 'o) Col.t * string rhs -> 'table t
  | In : ('table, 'a, 'o) Col.t * 'a list -> 'table t
  | Is : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | IsDistinct : ('table, 'a, 'o) Col.t * 'a rhs -> 'table t
  | Not : 'table t -> 'table t
  | Or : 'table t list -> 'table t
  | And : 'table t list -> 'table t

val encode_rhs :
  ?quote_simple_values:bool -> 'a Col.BaseTypes.ty -> 'a rhs -> string

val encode : 'table t -> string * string
val as_filter_list : 'a t -> 'a t list

module Syntax : sig
  val v : 'a -> 'a rhs
  val any : 'a list -> 'a rhs
  val all : 'a list -> 'a rhs
  val ( = ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( > ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( >= ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( < ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( <= ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( != ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( ~% ) : ('a, string, 'b) Col.t -> string rhs -> 'a t
  val ( ~~% ) : ('a, string, 'b) Col.t -> string rhs -> 'a t
  val ( == ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( !== ) : ('a, 'b, 'c) Col.t -> 'b rhs -> 'a t
  val ( ! ) : 'a t -> 'a t
  val ( || ) : 'a t -> 'a t -> 'a t
  val ( && ) : 'a t -> 'a t -> 'a t
end

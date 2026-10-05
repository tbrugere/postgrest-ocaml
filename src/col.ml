
type 'a enum = {
    name : string;                 
    to_string : 'a -> string;
    of_string : string -> 'a option;
  }

type _ base = 
    | Integer: int base
    | Numeric : float base
    | Bool : bool base
    | Str : string base
    | TimestampTz: Ptime.t base
    | JSON: Yojson.Safe.t base
    | Enum: 'a enum -> 'a base
;;

type _ ty = 
    | Nullable: 'a base -> ('a option) ty
    | Not_Null: 'a base -> 'a ty
;;

type ('table, 'a) col = {
    ty: 'a ty;
    name: string;
}

type (_, _) relationship_type = 
    | One: ('a, 'a option) relationship_type
    | Many: ('a, ('a list)) relationship_type
    | Exactly_one : ('a, 'a) relationship_type

type ('sourcetable, 'desttable, 'a, 'out) relationship = {
    ty: ('a, 'out) relationship_type; 
    name: string;
}

let encode_base: type a. a base -> a -> string =
    fun base value -> 
    match base with 
    | Integer -> string_of_int value
    | Numeric -> string_of_float value
    | Bool -> string_of_bool value
    | Str -> value
    | TimestampTz -> Ptime.to_rfc3339 value
    | JSON -> Yojson.Safe.to_string value
    | Enum enum -> enum.to_string value

let encode_value: type a. a ty -> a -> string =
    fun ty value ->
    match ty, value with 
    | (Nullable _), None -> "null"
    | (Nullable base), Some v -> encode_base base v 
    | (Not_Null base), v -> encode_base base v 


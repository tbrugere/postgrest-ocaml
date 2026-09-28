
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

type 'a enum = {
    name : string;                 
    to_string : 'a -> string;
    of_string : string -> 'a option;
  }

type _ ty = 
    | Integer: int ty
    | Numeric : float ty
    | Bool : bool ty
    | Str : string ty
    | TimestampTz: Ptime.t ty
    | JSON: Yojson.Safe.t ty
;;


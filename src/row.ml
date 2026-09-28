open Ty
open Col

type json = Yojson.Safe.t

type error = Missing | NotAnObject | WrongType

type columninfo = Name of string | Relationship of string * (columninfo list)

type ('table, 'a) t = {
    columns : columninfo list;                         (* for ?select= *)
    decode : json -> ('a, error) result;  (* one JSON object *)
  }

let decode_type : type a. a ty -> json -> (a, error) result =
   fun ty value -> match (ty, value) with
    | (Integer, `Int a) -> Ok a
    | (Integer, `Intlit a) -> Ok (int_of_string a)
    | (Numeric, `Float a) -> Ok (a)
    | (Bool, `Bool a) -> Ok (a)
    | (Str, `String a) -> Ok (a)
    | (TimestampTz, `String a) -> 
            begin match (Ptime.of_rfc3339 a) with
            | Ok (t, _, _) -> Ok t
            | _ -> Error WrongType
            end
    | (JSON, a) -> Ok a
    | _ -> Error WrongType

let get_row (name: string) : json -> (json, error) result = function
    | `Assoc obj ->  
        obj 
        |> List.assoc_opt name
        |> Option.to_result ~none:Missing
    | _ -> Error NotAnObject


let col: ('table, 'a) col -> ('table, 'a) t = function {ty; name} ->
    let columns = [Name name] in
    let decode (obj: json):  ('a, error) result = 
        let open Result.Syntax in 
        let* value = (get_row name obj) in
        decode_type ty value 
    in
    {columns; decode}

let rel: ('sourcetable, 'desttable) relationship -> ('desttable, 'a) t -> ('sourcetable, 'a) t =
    fun {name} {columns=inner_columns; decode=inner_decode} ->
    let columns = [Relationship (name, inner_columns)] in
    let decode (obj: json):  ('a, error) result = 
        let open Result.Syntax in 
        let* value = (get_row name obj) in
        inner_decode value 
    in
    {columns; decode}

let map (f: 'a -> 'b) (row: ('table, 'a) t) : ('table, 'b) t =
    {row with decode = ( fun x -> x |> row.decode |> Result.map f)}

let pair (row1: ('table, 'a) t) (row2: ('table, 'a) t): ('table, 'a * 'b) t =
    {
        columns = row1.columns @ row2.columns;
        decode = begin fun data ->
            let open Result.Syntax in
            let+ values1 = row1.decode data
            and+ values2 = row2.decode data
            in (values1, values2)
        end
    }



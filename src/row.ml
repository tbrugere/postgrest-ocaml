open Col

type json = Yojson.Safe.t

type error = Missing | NotAnObject | WrongType | UnexpectedNull

type columninfo = Name of string | Relationship of string * (columninfo list)

type ('table, 'a) t = {
    columns : columninfo list;                         (* for ?select= *)
    decode : json -> ('a, error) result;  (* one JSON object *)
  }

(** returns the first error if there is one *)
let result_all (results: ('a, 'err) result list): ('a list, 'err) result = 
    let rec inner revd_list = function
        | [] -> Ok (List.rev revd_list)
        | (Ok value)::rest -> inner (value::revd_list) rest
        | ((Error _) as err)::_ -> err
    in inner [] results


let decode_base: type a. a base -> json -> (a option, error) result = 
   fun base value -> 
       match (base, value) with
    | (_, `Null) -> Ok (None)
    | (Integer, `Int a) -> Ok (Some a)
    | (Integer, `Intlit a) -> Ok (Some (int_of_string a))
    | (Numeric, `Float a) -> Ok (Some a)
    | (Bool, `Bool a) -> Ok (Some a)
    | (Str, `String a) -> Ok (Some a)
    | (TimestampTz, `String a) -> 
            begin match (Ptime.of_rfc3339 a) with
            | Ok (t, _, _) -> Ok (Some t)
            | _ -> Error WrongType
            end
    | (JSON, a) -> Ok (Some a)
    | _ -> Error WrongType

let decode_type : type a. a ty -> json -> (a, error) result = 
    fun ty value -> 
    match ty with
    | Nullable base -> decode_base base value
    | Not_Null base -> 
        let open Result.Syntax in
        let* decoded = decode_base base value
        in Option.to_result ~none:UnexpectedNull decoded

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

let rel: type a out. ('sourcetable, 'desttable, a, out) relationship -> ('desttable, a) t -> ('sourcetable, out) t =
    fun {name; ty} {columns=inner_columns; decode=inner_decode} ->
    let columns = [Relationship (name, inner_columns)] in
    let open Result.Syntax in 
    let decode_value (value: json):  (out, error) result = 
        match ty, value with
        | (One, `Null) -> Ok None
        | (One, v) -> 
                let+ decoded = inner_decode v in Some decoded
        | (Exactly_one, `Null) -> Error UnexpectedNull
        | ( Exactly_one, v) -> inner_decode v
        | (Many, `List values) ->  
            values 
            |> List.map inner_decode
            |> result_all
        | _ -> Error WrongType
    in
    let decode (obj: json): (out, error) result =
        let* value = (get_row name obj) in
        decode_value value
    in
    {columns; decode}

let map (f: 'a -> 'b) (row: ('table, 'a) t) : ('table, 'b) t =
    {row with decode = ( fun x -> x |> row.decode |> Result.map f)}

let pair (row1: ('table, 'a) t) (row2: ('table, 'b) t): ('table, 'a * 'b) t =
    {
        columns = row1.columns @ row2.columns;
        decode = begin fun data ->
            let open Result.Syntax in
            let+ values1 = row1.decode data
            and+ values2 = row2.decode data
            in (values1, values2)
        end
    }


module Syntax = struct
    let (let+) row f = map f row
    let (and+) = pair
end

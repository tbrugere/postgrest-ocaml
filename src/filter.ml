open Col

type 'a rhs = 
    | Value of 'a
    | Any of 'a list
    | All of 'a list

type 'table t = 
    | Eq: ('table, 'a) col * 'a rhs -> 'table t
    | Gt: ('table, 'a) col * 'a rhs -> 'table t
    | Gte: ('table, 'a) col * 'a rhs -> 'table t
    | Lt: ('table, 'a) col * 'a rhs -> 'table t
    | Lte: ('table, 'a) col * 'a rhs -> 'table t
    | Neq: ('table, 'a) col * 'a rhs -> 'table t
    | Like: ('table, string) col * string rhs -> 'table t
    | ILike: ('table, string) col * string rhs -> 'table t
    | Match: ('table, string) col * string rhs -> 'table t
    | IMatch: ('table, string) col * string rhs -> 'table t
    | In: ('table, 'a) col * ('a list) -> 'table t
    | Is: ('table, 'a) col * 'a rhs -> 'table t
    | IsDistinct: ('table, 'a) col * 'a rhs -> 'table t
    (* TODO: fts ops and support tsvector col type *)
    (* TODO: set col types and operators*)
    (* TODO: range col types and operators*)
    | Not: 'table t -> 'table t
    | Or: 'table t list -> 'table t
    | And: 'table t list -> 'table t


let needs_escaping (s: string): bool = 
    s 
    |> String.to_seq
    |> Seq.exists (function ','|'.'|':'|'('|')' -> true | _ -> false)

let escape: string -> string = function
    | s when not (needs_escaping s) -> s
    | s ->
        let open Buffer in 
        let result = Buffer.create (String.length s + 7) in 
        let add = Buffer.add_char result in
        add '"';
        String.iter (function
            | ('\\' | '"') as c -> add '\\'; add c
            | c -> add c
        ) s;
        add '"';
        Buffer.contents result
    
let encode_list ?(left="(") ?(right=")") (ty: 'a ty) (values: 'a list) : string = 
    let inner = 
        values 
        |> List.map (encode_value ty)
        |> List.map escape
        |> String.concat ","
    in
    String.concat "" [left; inner; right]

let encode_rhs ?(quote_simple_values=false) (ty: 'a ty) (rhs: 'a rhs): string =
    match rhs with 
    (*never escape simple values. this is contrary to docs, but validated against the postgrest code*)
    | Value a when quote_simple_values -> escape {%string|.%{encode_value ty a}|}
    | Value a -> {%string|.%{encode_value ty a}|}
    | Any values ->  let array = encode_list ~left:"{" ~right:"}" ty values in 
                {%string|(any).%{array}|}
    | All values -> let array = encode_list ~left:"{" ~right:"}" ty values in 
                {%string|(all).%{array}|}

type encode_context = Default | InList
type param = Col of string | Combinator of string

let encode_op ?(context=Default) (col: ('table, 'a) col) (op_name: string) (rhs: 'a rhs) : (param * string) = 
    let encoded_rhs = encode_rhs ~quote_simple_values:(context=InList) col.ty rhs in
    Col col.name, {%string|%{op_name}%{encoded_rhs}|}

(*merges the param name and value, to convert eg 
    col=value into col.value
    or
    or=(...) into or(...)
    for use inside combinators
 *)
let flatten_param: param * string -> string = function
    | Col col_name, encoded_op -> {%string|%{escape col_name}.%{encoded_op}|}
    | Combinator combi, encoded_children -> {%string|%{combi}%{encoded_children}|}

(* returns a url parameter (param, value)*)
let rec encode_base ?(context=Default) (t: 'table t) :  param * string =  match t with
    | Eq (col, rhs) -> encode_op ~context col "eq" rhs
    | Gt (col, rhs) -> encode_op ~context col "gt" rhs
    | Gte (col, rhs) -> encode_op ~context col "gte" rhs
    | Lt (col, rhs) -> encode_op ~context col "lt" rhs
    | Lte (col, rhs) -> encode_op ~context col "lte" rhs
    | Neq (col, rhs) -> encode_op ~context col "neq" rhs
    | Like (col, rhs) -> encode_op ~context col "like" rhs
    | ILike (col, rhs) -> encode_op ~context col "ilike" rhs
    | Match (col, rhs) -> encode_op ~context col "match" rhs
    | IMatch (col, rhs) -> encode_op ~context col "imatch" rhs
    | In (col, values) -> Col col.name, {%string|in.%{encode_list col.ty values}|}
    | Is (col, rhs) -> encode_op ~context col "is" rhs
    | IsDistinct (col, rhs) -> encode_op ~context col "isdistinct" rhs
    (* TODO: fts ops and support tsvector col type *)
    (* TODO: set col types and operators*)
    (* TODO: range col types and operators*)
    | Not t -> begin match encode_base ~context:Default t with 
        | Col colname, inner -> Col colname, {%string|not.%{inner}|}
        | Combinator combi, inner -> Combinator {%string|not.%{combi}|}, inner
        end
    | Or children -> Combinator "or", encode_children children
    | And children -> Combinator "and", encode_children children
and encode_children (children: 'table t list) = 
    children
    |> List.map (encode_base ~context:InList)
    |> List.map flatten_param
    |> String.concat ","

let encode (t: 'table t): string * string = 
    let param, value =  encode_base ~context:Default t in
    let (Col param_text |  Combinator param_text) = param in 
    param_text, value


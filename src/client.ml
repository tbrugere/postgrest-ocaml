open Col.BaseTypes
open Util

type conn = { base_url : Uri.t; apikey : string; token : unit -> string option }

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type bad_response = RowError of int * Row.error | ExpectedArray | JsonError of string

type (_, _, _) card =
  | One : ('table, 'a) Row.t -> ('table, 'a, [< `Select | `Mutation ]) card
  | Many :
      ('table, 'a) Row.t
      -> ('table, 'a list, [< `Select | `Mutation ]) card
  | Zero : ('table, unit, [< `Mutation ]) card

let select_decode : type card_a cap.
    ('table, card_a, cap) card -> string -> (card_a, bad_response) result =
 fun card data ->
  let json =
    try Ok (Yojson.Safe.from_string data)
    with Yojson.Json_error err -> Error err
  in
  match (card, json) with
  | One row, Ok one_element_json ->
      one_element_json
      |> Row.decode row
      |> Result.map_error (fun error -> RowError (0, error))
  | Many row, Ok (`List l) ->
      l
      |> List.map (Row.decode row)
      |> add_index_to_errors
      |> result_all
      |> Result.map_error (fun (index, error) -> RowError (index, error))
  | Many _, Ok _ -> Error ExpectedArray
  | (One _ | Many _), Error err -> Error (JsonError err)
  | Zero, _ -> Ok ()

let accept_header_of_card : type card_a cap.
    ('table, card_a, cap) card -> (string * string) list = function
  | One _ -> [ ("Accept", "application/vnd.pgrst.object+json") ]
  | Many _ -> [ ("Accept", "application/json") ]
  | Zero -> []

let prefer_header : type card_a cap.
    query_type:[ `Select | `Mutation ] ->
    ?on_conflict:[ `Error | `Upsert | `Ignore ] ->
    ('table, card_a, cap) card ->
    string * string =
 fun ~query_type ?on_conflict card ->
  let return =
    match (query_type, card) with
    | `Mutation, One _ -> Some "return=representation"
    | `Mutation, Many _ -> Some "return=representation"
    | _ -> None
  and resolution =
    match on_conflict with
    | None | Some `Error -> None
    | Some `Upsert -> Some "resolution=merge-duplicates"
    | Some `Ignore -> Some "resolution=ignore-duplicates"
  and missing = Some "missing=default" in
  [ return; resolution; missing ]
  |> List.filter_map (fun x -> x)
  |> String.concat ","
  |> fun x -> ("Prefer", x)

let get_headers conn ~(query_type : [ `Select | `Mutation ])
    ?(on_conflict : [ `Error | `Upsert | `Ignore ] option)
    (response_card : ('table, 'card_a, [< `Select | `Mutation ]) card) :
    (string * string) list =
  let token_headers =
    match conn.token () with
    | None -> []
    | Some token -> [ ("auth", {%string|bearer %{token}|}) ]
  in
  let accept_headers = accept_header_of_card response_card in
  let prefer : (string * string) list =
    [ prefer_header ?on_conflict ~query_type response_card ]
  in
  token_headers @ accept_headers @ prefer

let build_uri : type a cap.
    table:'table table ->
    ?order:('table, 'b) Row.t ->
    ?where:'table Filter.t ->
    ('table, a, cap) card ->
    conn ->
    Uri.t =
 fun ~table ?order ?where card conn ->
  let filters =
    match where with
    | Some filters -> Filter.as_filter_list filters
    | None -> []
  in
  let filters_as_params = List.map Filter.encode filters in
  let table_name = Col.name_of_table table in
  let open Util.UrlUtils in
  let uri = conn.base_url / table_name in
  let uri = Uri.add_query_params' uri filters_as_params in
  let uri =
    match card with
    | Zero -> uri
    | One columns -> uri &? ("select", Row.encode_column_list columns)
    | Many columns -> uri &? ("select", Row.encode_column_list columns)
  in
  let uri =
    match order with
    | None -> uri
    | Some order_columns ->
        uri &? ("order", Row.encode_column_list order_columns)
  in
  uri

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type HTTP = sig
  module IO : IO

  type error

  val send :
    meth:[ `GET | `POST | `PATCH | `DELETE ] ->
    url:Uri.t ->
    headers:(string * string) list ->
    ?body:string ->
    unit ->
    (response, error) result IO.t
end

module Make (H : HTTP) = struct
  type nonrec conn = conn
  type nonrec response = response

  type error =
    | IOError of H.error
    | ErrorStatus of int
    | BadResponse of bad_response

  module IO = H.IO

  module IOSyntax = struct
    let ( let* ) = H.IO.bind

    let ( let+ ) : 'a H.IO.t -> ('a -> 'b) -> 'b H.IO.t =
     fun t f ->
      let* a = t in
      H.IO.return (f a)
  end

  let is_error_status = fun status -> status >= 400

  let decode_response :
      ('table, 'card_a, [< `Select | `Mutation ]) card ->
      (response, H.error) result ->
      ('card_a, error) result =
   fun card response_res ->
    match response_res with
    | Error ioerror -> Error (IOError ioerror)
    | Ok { status; _ } when is_error_status status -> Error (ErrorStatus status)
    | Ok { body; _ } ->
        select_decode card body
        |> Result.map_error (fun bad_response -> BadResponse bad_response)

  let select (conn : conn) ?(order : ('table, 'b) Row.t option)
      (card : ('table, 'card_a, [ `Select ]) card) (where : 'table Filter.t)
      (table : 'table table) : ('card_a, error) result H.IO.t =
    let uri = build_uri ~table ?order ~where card conn in
    let open IOSyntax in
    let+ response_res =
      H.send ~meth:`GET ~url:uri
        ~headers:(get_headers conn ~query_type:`Select card)
        ()
    in
    decode_response card response_res

  let insert (conn : conn)
      ?(on_conflict : [ `Error | `Upsert | `Ignore ] = `Error)
      ?(returning : ('table, 'card_a, [ `Mutation ]) card = Zero)
      ?(order : ('table, 'b) Row.t option) (input : ('table, [ `Full ]) Input.t)
      (table : 'table table) : ('card_a, error) result H.IO.t =
    let uri = build_uri ~table ?order returning conn in
    let open IOSyntax in
    let+ response_res =
      H.send ~meth:`POST ~url:uri
        ~headers:(get_headers conn ~query_type:`Mutation ~on_conflict returning)
        ~body:(Input.to_json_string input)
        ()
    in
    decode_response returning response_res

  let insert_batch (conn : conn)
      ?(on_conflict : [ `Error | `Upsert | `Ignore ] = `Error)
      ?(returning : ('table, 'card_a, [ `Mutation ]) card = Zero)
      ?(order : ('table, 'b) Row.t option)
      (inputs : ('table, [ `Full ]) Input.t list) (table : 'table table) :
      ('card_a, error) result H.IO.t =
    let uri = build_uri ~table ?order returning conn in
    let open IOSyntax in
    let+ response_res =
      H.send ~meth:`POST ~url:uri
        ~headers:(get_headers conn ~query_type:`Mutation ~on_conflict returning)
        ~body:(Input.list_to_json_string inputs)
        ()
    in
    decode_response returning response_res

  let update (conn : conn)
      ?(returning : ('table, 'card_a, [ `Mutation ]) card = Zero)
      ?(order : ('table, 'b) Row.t option)
      (input : ('table, [ `Partial ]) Input.t) (where : 'table Filter.t)
      (table : 'table table) : ('card_a, error) result H.IO.t =
    let uri = build_uri ~table ?order ~where returning conn in
    let open IOSyntax in
    let+ response_res =
      H.send ~meth:`PATCH ~url:uri
        ~headers:(get_headers conn ~query_type:`Mutation returning)
        ~body:(Input.to_json_string input)
        ()
    in
    decode_response returning response_res

  let delete (conn : conn)
      ?(returning : ('table, 'card_a, [ `Mutation ]) card = Zero)
      ?(order : ('table, 'b) Row.t option) (where : 'table Filter.t)
      (table : 'table table) : ('card_a, error) result H.IO.t =
    let uri = build_uri ~table ?order ~where returning conn in
    let open IOSyntax in
    let+ response_res =
      H.send ~meth:`DELETE ~url:uri
        ~headers:(get_headers conn ~query_type:`Mutation returning)
        ()
    in
    decode_response returning response_res
end

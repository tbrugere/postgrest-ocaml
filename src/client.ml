open Util

type conn = { 
    base_url : Uri.t; 
    apikey : string; 
    token : unit -> string option 
}

type response = { 
    status : int; 
    headers : (string * string) list; 
    body : string; 
}

type  bad_response = RowError of int * Row.error | ExpectedArray | JsonError

type (_, _, _) card =
    | One: ('table, 'a) Row.t -> ('table, 'a, [< `Select | `Mutation]) card
    | Many: ('table, 'a) Row.t -> ('table, 'a list, [< `Select | `Mutation] ) card
    | Zero: ('table, unit, [< `Mutation] ) card

let select_decode: type card_a. ('table, card_a, 'capability) card -> string -> (card_a, bad_response) result = 
    fun card data -> 
    let json = try  Some (Yojson.Safe.from_string data) with Yojson.Safe.Finally (_, _) -> None
    in
    match card, json with
    | One row, Some one_element_json -> 
        one_element_json
        |> row.decode
        |> Result.map_error (fun error -> RowError (0, error))
    | Many row, Some `List l ->
        l 
        |> List.map row.decode 
        |> add_index_to_errors 
        |> result_all
        |> Result.map_error (fun (index, error) -> RowError (index, error))
    | Many _, Some _ -> Error ExpectedArray
    | (One _|Many _), _ -> Error JsonError
    | Zero, _ -> Ok ()
            
let accept_header_of_card: type card_a. ('table, card_a, 'capability) card -> (string * string) list = function
    | One _ -> [("Accept", "application/vnd.pgrst.object+json")]
    | Many _ -> [("Accept", "application/json")]
    | Zero -> []


let get_headers conn (response_card: ('table, 'card_a, 'capability) card) : (string*string) list = 
    let token_headers = 
        match conn.token () with 
        | None -> []
        | Some token -> [("auth", {%string|bearer %{token}|})]
    in
    let accept_headers = accept_header_of_card response_card
    in 
    token_headers @ accept_headers

let build_uri: type a cap. ?order: ('table, 'b) Row.t ->  ?where: 'table Filter.t -> ('table, a, cap) card -> conn -> Uri.t = 
    fun ?order ?where card conn ->
    let filters = match where with 
        | Some filters -> Filter.as_filter_list filters 
        | None -> []
    in
    let filters_as_params = List.map Filter.encode filters in
    let uri = conn.base_url in
    let uri = Uri.add_query_params' uri filters_as_params in 
    let uri = match card with 
        | Zero -> uri
        | One columns -> Uri.add_query_param' uri ("select", Row.encode_column_list columns)
        | Many columns -> Uri.add_query_param' uri ("select", Row.encode_column_list columns)
    in
    let uri = Uri.add_query_params' uri filters_as_params in
    let uri = match order with 
        | None -> uri
        | Some order_columns -> Uri.add_query_param' uri ("order", Row.encode_column_list order_columns)
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
      ?body:string -> unit ->
      (response, error) result IO.t
end

module Make (H: HTTP) = struct
    type nonrec conn = conn
    type nonrec response = response
    type error = IOError of H.error | ErrorStatus of int | BadResponse of bad_response

    module IO = H.IO

    module IOSyntax = struct
        let (let*) = H.IO.bind
        let (let+): 'a H.IO.t -> ('a -> 'b) -> 'b H.IO.t = fun t f ->
            let* a = t in
            H.IO.return (f a)
    end

    let is_error_status = fun status -> status >= 400

    let decode_response (card: ('table, 'card_a, 'capability) card)  (response_res: (response, H.error) result): ('card_a, error) result = 
        match response_res with
        | Error ioerror -> Error (IOError ioerror)
        | Ok {status; _} when is_error_status status -> Error (ErrorStatus status)
        | Ok {body; _} ->
            select_decode card body
            |> Result.map_error (fun bad_response -> BadResponse bad_response)

    let select (conn: conn) ?(order: ('table, 'b) Row.t option) (card: ('table, 'card_a, [`Select]) card) (where: 'table Filter.t) : ('card_a, error) result H.IO.t = 
        let uri =   build_uri ?order ~where card conn in
        let open IOSyntax in
        let+ response_res = H.send ~meth:`GET ~url:uri ~headers:(get_headers conn ~response_card:card ()) () in 
        decode_response card response_res

    let insert (conn: conn) ?(returning: ('table, 'card_a, [`Mutation]) card = Zero) ?(order: ('table, 'b) Row.t option) : ('card_a, error) result H.IO.t = 


    
end

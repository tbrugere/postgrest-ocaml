type conn = {base_url : Uri.t; headers : unit -> (string * string) list}

val supabase_conn :
  Uri.t -> apikey:string -> token:(unit -> string option) -> conn

type response = {status : int; headers : (string * string) list; body : string}

type bad_response =
  | RowError of int * Row.error
  | ExpectedArray
  | JsonError of string

type (_, _, _) card =
  | One : ('table, 'a) Row.t -> ('table, 'a, [< `Mutation | `Select]) card
  | Many : ('table, 'a) Row.t -> ('table, 'a list, [< `Mutation | `Select]) card
  | Zero : ('table, unit, [< `Mutation]) card

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type HTTP = sig
  module IO : IO

  type error

  val send :
    meth:[`DELETE | `GET | `PATCH | `POST] ->
    url:Uri.t ->
    headers:(string * string) list ->
    ?body:string ->
    unit ->
    (response, error) result IO.t
end

module Make : (H : HTTP) -> sig
  type nonrec conn = conn
  type nonrec response = response

  type error =
    | IOError of H.error
    | ErrorStatus of {status : int; body : string}
    | BadResponse of bad_response

  val select :
    conn ->
    ?order:('table, 'b) Row.t ->
    ('table, 'card_a, [`Select]) card ->
    'table Filter.t ->
    'table Col.BaseTypes.table ->
    ('card_a, error) result H.IO.t

  val insert :
    conn ->
    ?on_conflict:[`Error | `Ignore | `Upsert] ->
    returning:('table, unit, [`Mutation]) card ->
    ?order:('table, 'b) Row.t ->
    ('table, [`Full]) Input.t ->
    'table Col.BaseTypes.table ->
    (unit, error) result H.IO.t

  val insert_batch :
    conn ->
    ?on_conflict:[`Error | `Ignore | `Upsert] ->
    returning:('table, unit, [`Mutation]) card ->
    ?order:('table, 'b) Row.t ->
    ('table, [`Full]) Input.t list ->
    'table Col.BaseTypes.table ->
    (unit, error) result H.IO.t

  val update :
    conn ->
    returning:('table, unit, [`Mutation]) card ->
    ?order:('table, 'b) Row.t ->
    ('table, [`Partial]) Input.t ->
    'table Filter.t ->
    'table Col.BaseTypes.table ->
    (unit, error) result H.IO.t

  val delete :
    conn ->
    returning:('table, unit, [`Mutation]) card ->
    ?order:('table, 'b) Row.t ->
    'table Filter.t ->
    'table Col.BaseTypes.table ->
    (unit, error) result H.IO.t
end

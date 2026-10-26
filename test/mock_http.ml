(* An HTTP backend for Postgrest.Client.Make that performs no IO: the
   identity monad, a recorded list of requests, and a responder the test
   installs before each call. *)

module IO = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
end

type error = string

type request = {
  meth : [ `DELETE | `GET | `PATCH | `POST ];
  url : Uri.t;
  headers : (string * string) list;
  body : string option;
}

let requests : request list ref = ref []

let responder : (request -> (Postgrest.Client.response, error) result) ref =
  ref (fun _ -> Error "no responder installed")

let send ~meth ~url ~headers ?body () =
  let request = { meth; url; headers; body } in
  requests := request :: !requests;
  !responder request

(* Test-side helpers. *)

let reset () =
  requests := [];
  responder := fun _ -> Error "no responder installed"

let last () =
  match !requests with
  | [] -> failwith "mock_http: no request was sent"
  | r :: _ -> r

let respond ?(status = 200) ?(headers = []) body =
  responder := fun _ -> Ok { Postgrest.Client.status; headers; body }

let fail_with (e : error) = responder := fun _ -> Error e

let string_of_meth = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PATCH -> "PATCH"
  | `DELETE -> "DELETE"

(* path?query with the query percent-decoded, for comparison against the
   unencoded form the scenarios are written in. *)
let decoded_target (url : Uri.t) =
  match Uri.verbatim_query url with
  | None -> Uri.path url
  | Some q -> Uri.path url ^ "?" ^ Uri.pct_decode q

let header name (r : request) =
  List.find_map
    (fun (k, v) ->
      match
        String.equal (String.lowercase_ascii k) (String.lowercase_ascii name)
      with
      | true -> Some v
      | false -> None)
    r.headers

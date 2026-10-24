let add_index_to_errors (results : ('a, 'err) result list) :
    ('a, int * 'err) result list =
  results
  |> List.mapi (fun i res ->
      match res with Ok a -> Ok a | Error err -> Error (i, err))

(** returns the first error if there is one *)
let result_all (results : ('a, 'err) result list) : ('a list, 'err) result =
  let rec inner revd_list = function
    | [] -> Ok (List.rev revd_list)
    | Ok value :: rest -> inner (value :: revd_list) rest
    | (Error _ as err) :: _ -> err
  in
  inner [] results

module UrlUtils = struct
  let append_segment base segment =
    let path = Uri.path base in
    let path =
      match String.ends_with ~suffix:"/" path with
      | true -> path ^ Uri.pct_encode segment
      | false -> path ^ "/" ^ Uri.pct_encode segment
    in
    Uri.with_path base path

  let ( / ) = append_segment
  let ( &? ) = Uri.add_query_param'
end

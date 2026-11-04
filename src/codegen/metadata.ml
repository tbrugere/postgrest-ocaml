type t = (string * string option) list

let prefix = "metadata:"

(* A look-alike (indented, capitalised) is an error rather than ignored. *)
let looks_like_metadata line =
  match String.index_opt line ':' with
  | Some colon ->
      String.lowercase_ascii (String.trim (String.sub line 0 colon))
      = "metadata"
  | None -> false

let entry token =
  match String.index_opt token '=' with
  | None -> Some (token, None)
  | Some eq -> (
      match String.sub token (eq + 1) (String.length token - eq - 1) with
      | "" -> None
      | value -> Some (String.sub token 0 eq, Some value))

let entries line =
  let tokens =
    String.sub line (String.length prefix)
      (String.length line - String.length prefix)
    |> String.map (function ',' | '\t' | '\r' -> ' ' | c -> c)
    |> String.split_on_char ' '
    |> List.filter (fun token -> token <> "")
  in
  let entries = List.filter_map entry tokens in
  let keys = List.sort_uniq String.compare (List.map fst entries) in
  match List.length keys = List.length tokens with
  | true -> Some entries
  | false -> None

let of_comment comment =
  let lines = String.split_on_char '\n' (Option.value ~default:"" comment) in
  match List.filter looks_like_metadata lines with
  | [] -> Some []
  | [line] when String.starts_with ~prefix line -> entries line
  | _ -> None

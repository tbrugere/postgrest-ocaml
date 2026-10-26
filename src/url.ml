open Col

let needs_escaping (s : string) : bool =
  s
  |> String.to_seq
  |> Seq.exists (function ',' | '.' | ':' | '(' | ')' -> true | _ -> false)

let escape : string -> string = function
  | s when not (needs_escaping s) -> s
  | s ->
      let result = Buffer.create (String.length s + 7) in
      let add = Buffer.add_char result in
      add '"';
      String.iter
        (function
          | ('\\' | '"') as c ->
              add '\\';
              add c
          | c -> add c)
        s;
      add '"';
      Buffer.contents result

let encode_list ?(left = "(") ?(right = ")") (ty : 'a ty) (values : 'a list) :
    string =
  let inner =
    values |> List.map (encode_value ty) |> List.map escape |> String.concat ","
  in
  String.concat "" [ left; inner; right ]

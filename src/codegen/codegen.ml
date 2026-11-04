let generate ?module_prefix text =
  let module_prefix =
    match module_prefix with
    | None -> []
    | Some prefix -> String.split_on_char '.' prefix
  in
  Result.bind (Catalog.of_string text) (fun catalog ->
      Result.map Emit.to_string (Model.of_catalog ~module_prefix catalog))

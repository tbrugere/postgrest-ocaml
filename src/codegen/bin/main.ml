(* postgrest-codegen [--module-prefix M] [-o FILE] CATALOG.json: the
   Postgrest.Col declarations of the tables in catalog.sql's output, on stdout
   or in FILE. Exit 2 on a bad argument, an unreadable file or a catalog the
   rules do not cover; nothing is written then. *)

let usage = "postgrest-codegen [--module-prefix MODULE] [-o FILE] CATALOG.json"

let read file =
  try Ok (In_channel.with_open_bin file In_channel.input_all)
  with Sys_error message -> Error message

let write output text =
  try
    match output with
    | None -> Ok (print_string text)
    | Some file ->
        Ok (Out_channel.with_open_bin file (fun oc -> output_string oc text))
  with Sys_error message -> Error message

let run ?module_prefix ~output = function
  | [file] -> (
      match read file with
      | Error _ as e -> e
      | Ok text -> (
          match Postgrest_codegen.Codegen.generate ?module_prefix text with
          | Error e -> Error (file ^ ": " ^ Postgrest_codegen.Error.to_string e)
          | Ok text -> write output text))
  | [] | _ :: _ :: _ -> Error "give exactly one catalog file"

let () =
  let module_prefix = ref None and output = ref None and files = ref [] in
  let specs =
    [
      ( "--module-prefix",
        Arg.String (fun m -> module_prefix := Some m),
        "MODULE  the module the metadata's module paths are under, e.g. \
         My_domain" );
      ( "-o",
        Arg.String (fun f -> output := Some f),
        "FILE  write to FILE, not stdout" );
    ]
  in
  (* style: Arg's callbacks are its interface; the refs never leave here. *)
  match Arg.parse_argv Sys.argv specs (fun f -> files := f :: !files) usage with
  | exception Arg.Help text ->
      print_string text;
      exit 0
  | exception Arg.Bad text ->
      prerr_string text;
      exit 2
  | () -> (
      match
        run ?module_prefix:!module_prefix ~output:!output (List.rev !files)
      with
      | Ok () -> ()
      | Error message ->
          prerr_endline ("error: " ^ message);
          exit 2)

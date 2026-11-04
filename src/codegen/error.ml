type t =
  | Bad_json of string
  | Bad_table of {index : int; path : string}
  | Malformed_metadata of {table : string; column : string option}
  | Unknown_metadata of {table : string; column : string option; key : string}
  | Unsupported_type of {table : string; column : string; sql_type : string}
  | Uuid_without_id of {table : string; column : string}
  | Lookup_without_ocaml of {table : string; column : string; lookup : string}
  | Generated_column of {table : string; column : string}
  | Ambiguous_relationship of {table : string; column : string; other : string}
  | Unique_foreign_key of {table : string; column : string}

let place table = function
  | None -> Printf.sprintf "table %s" table
  | Some column -> Printf.sprintf "column %s.%s" table column

let to_string = function
  | Bad_json message -> String.map (function '\n' -> ' ' | c -> c) message
  | Bad_table {index; path} ->
      Printf.sprintf "value %d: not a table object: %s" index path
  | Malformed_metadata {table; column} ->
      Printf.sprintf "%s: unreadable metadata line" (place table column)
  | Unknown_metadata {table; column; key} ->
      Printf.sprintf "%s: unknown metadata %S" (place table column) key
  | Unsupported_type {table; column; sql_type} ->
      Printf.sprintf "%s: unsupported type %s"
        (place table (Some column))
        sql_type
  | Uuid_without_id {table; column} ->
      Printf.sprintf
        "%s: a uuid column must be the key of, or reference, a table with id="
        (place table (Some column))
  | Lookup_without_ocaml {table; column; lookup} ->
      Printf.sprintf "%s: lookup table %s has no ocaml="
        (place table (Some column))
        lookup
  | Generated_column {table; column} ->
      Printf.sprintf "%s: generated columns are not supported"
        (place table (Some column))
  | Ambiguous_relationship {table; column; other} ->
      Printf.sprintf
        "%s: a second foreign key between %s and %s makes embedding ambiguous"
        (place table (Some column))
        table other
  | Unique_foreign_key {table; column} ->
      Printf.sprintf
        "%s: a unique foreign key needs no_reverse (the reverse is not a list)"
        (place table (Some column))

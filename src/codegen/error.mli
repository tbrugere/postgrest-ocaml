(** Everything that stops generation. Only what would generate wrong OCaml, or
    is ambiguous, is checked; a name that does not compile is left to the
    compiler, which reads the output anyway. *)

type t =
  | Bad_json of string  (** Yojson's message, with the line. *)
  | Bad_table of {index : int; path : string}
      (** The [index]th value (from 1) is not a table object: a field missing,
          mistyped or unknown, its own or a column's. [path] is
          [ppx_deriving_yojson]'s, e.g. [Catalog.column.not_null]. *)
  | Malformed_metadata of {table : string; column : string option}
      (** Several [metadata:] lines (or one that only looks like one), a key
          given twice, or a [key=] without a value. *)
  | Unknown_metadata of {table : string; column : string option; key : string}
  | Unsupported_type of {table : string; column : string; sql_type : string}
  | Uuid_without_id of {table : string; column : string}
      (** A [uuid] column that is neither the key of a table with [id=] nor a
          foreign key to one. *)
  | Lookup_without_ocaml of {table : string; column : string; lookup : string}
  | Generated_column of {table : string; column : string}
      (** PostgREST rejects a write to it, which its column would allow. *)
  | Ambiguous_relationship of {table : string; column : string; other : string}
      (** A second foreign key between the same two tables: PostgREST cannot
          tell which one an embed means. *)
  | Unique_foreign_key of {table : string; column : string}
      (** PostgREST embeds the reverse of a unique foreign key as an object, not
          the list a [Many] decodes. *)

val to_string : t -> string

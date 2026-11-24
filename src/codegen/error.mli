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
  | Empty_lookup of {table : string}
      (** A lookup table without [ocaml=] and without values: its generated type
          would have no constructor. *)
  | Bad_lookup_value of {table : string; value : string}
      (** A value of a lookup table without [ocaml=] that is not a constructor
          once capitalised: a letter, then letters, digits, [_] or [']. Two
          values that capitalise the same are left to the compiler. *)
  | Generated_column of {table : string; column : string}
      (** PostgREST rejects a write to it, which its column would allow. *)
  | Ambiguous_relationship of {table : string; column : string; other : string}
      (** A second foreign key between the same two tables: PostgREST cannot
          tell which one an embed means. *)
  | Unique_foreign_key of {table : string; column : string}
      (** PostgREST embeds the reverse of a unique foreign key as an object, not
          the list a [Many] decodes. *)

val to_string : t -> string

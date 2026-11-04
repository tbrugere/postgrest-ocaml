(** The OCaml of a {!Model.t}, built as a Ppxlib AST and printed with Pprintast.
*)

val structure : Model.t -> Ppxlib.structure
(** [[@@@ocamlformat "disable"]], [module Col = Postgrest.Col], one abstract
    type per table (so that relationships can name any table), the codecs, then
    one module per table with its [table], columns and relationships. *)

val to_string : Model.t -> string
(** {!structure} printed, after a header comment saying the file is generated.
*)

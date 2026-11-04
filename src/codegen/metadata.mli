(** The [metadata:] line of a table or column comment: what the SQL cannot say,
    as [key=value] pairs and bare flags separated by commas or spaces, e.g.
    [metadata: kind=lookup, ocaml=Reference.Status]. The line starts the comment
    or follows a newline in it; the rest of the comment is free text. Which keys
    mean what is decided by {!Model}. *)

type t = (string * string option) list
(** Keys in the order written, with their value; [None] for a bare flag. *)

val of_comment : string option -> t option
(** [Some []] without a [metadata:] line; [None] if more than one line looks
    like one (["  metadata:"] and ["Metadata:"] count), a key is given twice, or
    a [key=] has no value. *)

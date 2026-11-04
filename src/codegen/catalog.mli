(** The output of [catalog.sql]: a stream of JSON objects, one per table,
    decoded by [ppx_deriving_yojson]. A field missing, mistyped or unknown is an
    error. *)

type column = {
  name : string;
  sql_type : string;  (** As [format_type] writes it, e.g. [text[]]. *)
  not_null : bool;
  has_default : bool;  (** A default or an identity. *)
  is_generated : bool;
  is_primary_key : bool;  (** The table's primary key, alone. *)
  is_unique : bool;  (** Unique alone, primary keys included. *)
  comment : string option;
  foreign_key : string option;
      (** The table of the same schema a single-column foreign key on it
          references (the first by constraint name). *)
}

type table = {
  name : string;
  comment : string option;
  columns : column list;  (** In column order. *)
  values : string list option;
      (** A lookup table's values, sorted; [None] for any other table. *)
}

type t = table list
(** In the order of the stream. *)

val of_string : string -> (t, Error.t) result
(** The first value that is not JSON, or not a table object, is the error. *)

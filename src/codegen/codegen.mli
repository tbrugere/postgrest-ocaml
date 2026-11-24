(** A catalog to the [Postgrest.Col] declarations of its tables: {!Catalog}
    reads it, {!Model} applies the metadata rules documented there, {!Emit}
    prints the OCaml. *)

val generate :
  ?module_prefix:string ->
  ?generate_ids:bool ->
  string ->
  (string, Error.t) result
(** [generate ~module_prefix catalog] is the generated OCaml for [catalog], the
    output of [catalog.sql]. Metadata module paths are taken under
    [module_prefix] (e.g. ["My_domain"]: [id=Ids.User_id] types the key
    [My_domain.Ids.User_id.t]); without it, as written. [generate_ids] (default
    [true]): whether a table with a [uuid] key and no [id=] gets a generated
    [<Table>_id] module (see {!Model}). *)

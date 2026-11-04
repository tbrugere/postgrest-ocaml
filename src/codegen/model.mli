(** What the emitter needs, from the catalog and the metadata rules.

    Table metadata:
    - [kind=lookup, ocaml=<Module>]: a lookup table. It gets no module; a column
      with a foreign key to it is typed [<Module>.t].
    - [id=<Module>]: the table's [uuid] primary key, and every [uuid] foreign
      key to it, is typed [<Module>.t].

    Column metadata, bare flags:
    - [server_only]: the column is left out.
    - [no_reverse]: no relationship back from the referenced table.

    [<Module>] is a module path under the [module_prefix] with a type [t],
    [to_json] and [of_json], passed as they are to a [Postgrest.Col.custom]. A
    foreign key to another generated table gives a relationship named after the
    column without its [_id] (after the referenced table if it has none; [One]
    if nullable, [Exactly_one] if [NOT NULL]), and the referenced table a [Many]
    named after this table. What {!Error.t} lists is rejected. *)

type source = Id | Lookup of string  (** The lookup table. *)

type codec = {
  binding : string;  (** [Ids.Plant_id] gives [ids_plant_id]. *)
  path : string list;  (** The module, prefix included. *)
  source : source;
}

type base =
  | Str
  | Integer
  | Numeric
  | Bool
  | Timestamp_tz
  | Json
  | Custom of codec

(** [Required]: [NOT NULL] without a default, so an insert must give it.
    [Defaulted]: [NOT NULL] with a default. *)
type value = Required | Defaulted | Nullable

type column = {name : string; base : base; value : value}
type cardinality = One | Exactly_one | Many

type relationship = {
  binding : string;
  target : string;  (** The table the relationship embeds. *)
  cardinality : cardinality;
}

type table = {
  name : string;
  columns : column list;
  relationships : relationship list;  (** Forward ones, then reverse ones. *)
}

type t = {
  codecs : codec list;  (** Lookup codecs in catalog order, then id codecs. *)
  tables : table list;  (** By name; lookup tables excluded. *)
}

val of_catalog : module_prefix:string list -> Catalog.t -> (t, Error.t) result
(** Tables, codecs and relationships in the order of the catalog. *)

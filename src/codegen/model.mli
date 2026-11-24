(** What the emitter needs, from the catalog and the metadata rules.

    Table metadata:
    - [kind=lookup]: a lookup table. It gets no table module; a column with a
      foreign key to it is typed [<Table>.t], where [<Table>] is the table's
      name capitalised and is generated from the table's values: one constant
      constructor per value, each value capitalised (see {!lookup}).
    - [kind=lookup, ocaml=<Module>]: the same, but [<Module>] is written by hand
      and nothing is generated; the column is typed [<Module>.t].
    - A table whose primary key is a single [uuid] column gets a generated
      [<Table>_id] module ([authors] gives [Authors_id], see {!t.id_modules}):
      its key, and every [uuid] foreign key to it, is typed [<Table>_id.t].
      [generate_ids:false] turns that default off.
    - [generate_id=true|false]: overrides that default for the table; [true]
      needs a single [uuid] primary key.
    - [id=<Module>]: the same typing, but [<Module>] is written by hand and
      nothing is generated. Any other [uuid] column is a [string].

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
  path : string list;
      (** The module, prefix included; a generated lookup module has none. *)
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

type lookup = {
  module_name : string;  (** The table's name, capitalised. *)
  constructors : (string * string) list;
      (** [(constructor, value)], in the catalog's (sorted) order. *)
}
(** The module generated for a lookup table without [ocaml=]. *)

type t = {
  lookups : lookup list;  (** Generated lookup modules, in catalog order. *)
  id_modules : string list;
      (** Generated id modules ([Authors_id]), in catalog order: an abstract [t]
          over any string, with [of_string], [to_string], [equal], [compare],
          [to_json] and [of_json]. *)
  codecs : codec list;  (** Lookup codecs in catalog order, then id codecs. *)
  tables : table list;  (** By name; lookup tables excluded. *)
}

val of_catalog :
  ?generate_ids:bool ->
  module_prefix:string list ->
  Catalog.t ->
  (t, Error.t) result
(** Tables, codecs and relationships in the order of the catalog. *)

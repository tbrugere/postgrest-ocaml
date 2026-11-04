type source = Id | Lookup of string
type codec = {binding : string; path : string list; source : source}

type base =
  | Str
  | Integer
  | Numeric
  | Bool
  | Timestamp_tz
  | Json
  | Custom of codec

type value = Required | Defaulted | Nullable
type column = {name : string; base : base; value : value}
type cardinality = One | Exactly_one | Many

type relationship = {
  binding : string;
  target : string;
  cardinality : cardinality;
}

type table = {
  name : string;
  columns : column list;
  relationships : relationship list;
}

type t = {codecs : codec list; tables : table list}

let ( let* ) = Result.bind

let rec map_result f acc = function
  | [] -> Ok (List.rev acc)
  | x :: rest ->
      let* y = f x in
      map_result f (y :: acc) rest

let plain_type = function
  | "text" -> Some Str
  | "integer" -> Some Integer
  | "double precision" -> Some Numeric
  | "boolean" -> Some Bool
  | "timestamp with time zone" -> Some Timestamp_tz
  | "jsonb" | "text[]" -> Some Json
  | _ -> None

let token = function key, None -> key | key, Some value -> key ^ "=" ^ value

type kind = Lookup_table of string option | Entity of string option

let kind (t : Catalog.table) =
  match Metadata.of_comment t.comment with
  | None -> Error (Error.Malformed_metadata {table = t.name; column = None})
  | Some entries -> (
      let lookup = List.mem ("kind", Some "lookup") entries in
      let allowed = function
        | "kind", Some "lookup" | "ocaml", Some _ -> lookup
        | "id", Some _ -> not lookup
        | _ -> false
      in
      let value key = Option.join (List.assoc_opt key entries) in
      match (List.find_opt (fun e -> not (allowed e)) entries, lookup) with
      | Some e, _ ->
          Error
            (Error.Unknown_metadata
               {table = t.name; column = None; key = token e})
      | None, true -> Ok (t.name, Lookup_table (value "ocaml"))
      | None, false -> Ok (t.name, Entity (value "id")))

type flag = Plain | Server_only | No_reverse

let flag table (c : Catalog.column) =
  let column = Some c.name in
  match Metadata.of_comment c.comment with
  | None -> Error (Error.Malformed_metadata {table; column})
  | Some entries -> (
      let flags = [("server_only", None); ("no_reverse", None)] in
      match List.find_opt (fun e -> not (List.mem e flags)) entries with
      | Some e -> Error (Error.Unknown_metadata {table; column; key = token e})
      | None when List.mem_assoc "server_only" entries -> Ok (c, Server_only)
      | None when List.mem_assoc "no_reverse" entries -> Ok (c, No_reverse)
      | None -> Ok (c, Plain))

type context = {
  lookups : (string * codec option) list;
  ids : (string * codec) list;
  entities : string list;
  synced : (string * (Catalog.column * flag) list) list;
      (* The entities' columns but server_only ones, by table, in catalog
         order. *)
}

(* The generated table a foreign key on [c] embeds, if any. *)
let embeds ctx (c : Catalog.column) =
  match c.foreign_key with
  | Some target when List.mem target ctx.entities -> Some target
  | Some _ | None -> None

let base ctx table (c : Catalog.column) =
  let uuid_without_id () =
    Error (Error.Uuid_without_id {table; column = c.name})
  in
  let id_of referenced =
    match List.assoc_opt referenced ctx.ids with
    | Some codec -> Ok (Custom codec)
    | None -> uuid_without_id ()
  in
  let plain () =
    match plain_type c.sql_type with
    | Some base -> Ok base
    | None ->
        Error
          (Error.Unsupported_type
             {table; column = c.name; sql_type = c.sql_type})
  in
  match (c.foreign_key, c.sql_type) with
  | _ when c.is_generated ->
      Error (Error.Generated_column {table; column = c.name})
  | Some target, sql_type -> (
      match (List.assoc_opt target ctx.lookups, sql_type) with
      | Some (Some codec), _ -> Ok (Custom codec)
      | Some None, _ ->
          Error
            (Error.Lookup_without_ocaml
               {table; column = c.name; lookup = target})
      | None, "uuid" -> id_of target
      | None, _ -> plain ())
  | None, "uuid" when c.is_primary_key -> id_of table
  | None, "uuid" -> uuid_without_id ()
  | None, _ -> plain ()

let column ctx table ((c : Catalog.column), _) =
  let* base = base ctx table c in
  let value =
    match (c.not_null, c.has_default) with
    | true, false -> Required
    | true, true -> Defaulted
    | false, (true | false) -> Nullable
  in
  Ok {name = c.name; base; value}

let forward (c : Catalog.column) target =
  let binding =
    match String.ends_with ~suffix:"_id" c.name with
    | true -> String.sub c.name 0 (String.length c.name - 3)
    | false -> target
  in
  let cardinality =
    match c.not_null with true -> Exactly_one | false -> One
  in
  {binding; target; cardinality}

let table ctx (name, own) =
  let* columns = map_result (column ctx name) [] own in
  let forward =
    List.filter_map (fun (c, _) -> Option.map (forward c) (embeds ctx c)) own
  in
  let reverse =
    List.concat_map
      (fun (from, columns) ->
        List.filter_map
          (fun (c, flag) ->
            match (embeds ctx c, flag) with
            | Some target, Plain when target = name ->
                Some {binding = from; target = from; cardinality = Many}
            | (Some _ | None), (Plain | Server_only | No_reverse) -> None)
          columns)
      ctx.synced
  in
  Ok {name; columns; relationships = forward @ reverse}

(* PostgREST names an embed by table, so two foreign keys between the same
   two tables are ambiguous (PGRST201), server_only ones included: PostgREST
   sees them. [flagged] is [synced] with the server_only columns kept. *)
let check_foreign_keys ctx flagged =
  let rec go seen = function
    | [] -> Ok ()
    | (table, (c : Catalog.column), flag) :: rest -> (
        match (embeds ctx c, flag) with
        | None, (Plain | Server_only | No_reverse) -> go seen rest
        | Some target, (Plain | Server_only | No_reverse)
          when List.mem (List.sort String.compare [table; target]) seen ->
            Error
              (Error.Ambiguous_relationship
                 {table; column = c.name; other = target})
        | Some _, Plain when c.is_unique ->
            Error (Error.Unique_foreign_key {table; column = c.name})
        | Some target, (Plain | Server_only | No_reverse) ->
            go (List.sort String.compare [table; target] :: seen) rest)
  in
  go []
    (List.concat_map
       (fun (table, columns) ->
         List.map (fun (c, flag) -> (table, c, flag)) columns)
       flagged)

let of_catalog ~module_prefix (catalog : Catalog.t) =
  let* kinds = map_result kind [] catalog in
  let codec source path =
    let segments = String.split_on_char '.' path in
    let binding = String.lowercase_ascii (String.concat "_" segments) in
    {binding; path = module_prefix @ segments; source}
  in
  let lookups =
    List.filter_map
      (function
        | name, Lookup_table ocaml ->
            Some (name, Option.map (codec (Lookup name)) ocaml)
        | _, Entity _ -> None)
      kinds
  in
  let ids =
    List.filter_map
      (function
        | name, Entity (Some id) -> Some (name, codec Id id)
        | _, (Entity None | Lookup_table _) -> None)
      kinds
  in
  let entities =
    List.filter_map
      (function name, Entity _ -> Some name | _, Lookup_table _ -> None)
      kinds
  in
  let* flagged =
    map_result
      (fun (t : Catalog.table) ->
        let* columns = map_result (flag t.name) [] t.columns in
        Ok (t.name, columns))
      []
      (List.filter
         (fun (t : Catalog.table) -> List.mem t.name entities)
         catalog)
  in
  let synced =
    List.map
      (fun (name, columns) ->
        ( name,
          List.filter
            (function
              | _, Server_only -> false | _, (Plain | No_reverse) -> true)
            columns ))
      flagged
  in
  let ctx = {lookups; ids; entities; synced} in
  let* () = check_foreign_keys ctx flagged in
  let* tables = map_result (table ctx) [] synced in
  Ok {codecs = List.filter_map snd lookups @ List.map snd ids; tables}

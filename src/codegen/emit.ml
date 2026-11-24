open Ppxlib

let loc = Location.none

module B = Ast_builder.Make (struct
  let loc = loc
end)

let lid = function
  | [] -> invalid_arg "Emit.lid: empty path"
  | first :: rest ->
      {
        txt = List.fold_left (fun acc s -> Ldot (acc, s)) (Lident first) rest;
        loc;
      }

let type_of_path path = B.ptyp_constr (lid (path @ ["t"])) []
let value_of_path path name = B.pexp_ident (lid (path @ [name]))

let codec_name (c : Model.codec) =
  match c.source with Id -> "uuid" | Lookup table -> table

let codec (c : Model.codec) =
  [%stri
    let [%p B.pvar c.binding] : [%t type_of_path c.path] Col.custom =
      {
        name = [%e B.estring (codec_name c)];
        to_json = [%e value_of_path c.path "to_json"];
        of_json = [%e value_of_path c.path "of_json"];
      }]

let base_type : Model.base -> core_type = function
  | Str -> [%type: string]
  | Integer -> [%type: int]
  | Numeric -> [%type: float]
  | Bool -> [%type: bool]
  | Timestamp_tz -> [%type: Ptime.t]
  | Json -> [%type: Yojson.Safe.t]
  | Custom c -> type_of_path c.path

let base_expression : Model.base -> expression = function
  | Str -> [%expr Str]
  | Integer -> [%expr Integer]
  | Numeric -> [%expr Numeric]
  | Bool -> [%expr Bool]
  | Timestamp_tz -> [%expr TimestampTz]
  | Json -> [%expr JSON]
  | Custom c -> [%expr Custom [%e B.evar c.binding]]

let column ({name; base; value} : Model.column) =
  let base_t = base_type base and base_e = base_expression base in
  let value_t, ty, optional =
    match value with
    | Required -> (base_t, [%expr Not_Null [%e base_e]], [%type: [`Mandatory]])
    | Defaulted -> (base_t, [%expr Not_Null [%e base_e]], [%type: [`Optional]])
    | Nullable ->
        ( [%type: [%t base_t] option],
          [%expr Nullable [%e base_e]],
          [%type: [`Optional]] )
  in
  [%stri
    let [%p B.pvar name] : (t, [%t value_t], [%t optional]) Col.t =
      {ty = [%e ty]; name = [%e B.estring name]}]

let relationship ({binding; target; cardinality} : Model.relationship) =
  let target_t = B.ptyp_constr (lid [target]) [] in
  let out, ty =
    match cardinality with
    | One -> ([%type: 'a option], [%expr One])
    | Exactly_one -> ([%type: 'a], [%expr Exactly_one])
    | Many -> ([%type: 'a list], [%expr Many])
  in
  [%stri
    let [%p B.pvar binding] : (t, [%t target_t], 'a, [%t out]) Col.relationship
        =
      {ty = [%e ty]; name = [%e B.estring target]}]

let table_module ({name; columns; relationships} : Model.table) =
  let items =
    [%stri type t = [%t B.ptyp_constr (lid [name]) []]]
    :: [%stri let table : t Col.table = Col.table [%e B.estring name]]
    :: List.map column columns
    @ List.map relationship relationships
  in
  B.pstr_module
    (B.module_binding
       ~name:{txt = Some (String.capitalize_ascii name); loc}
       ~expr:(B.pmod_structure items))

let constant name = B.pexp_construct (lid [name]) None
let constant_pattern name = B.ppat_construct (lid [name]) None

(* type t = A | B, all, to_string, of_string, to_json, of_json. *)
let lookup_module ({module_name; constructors} : Model.lookup) =
  let t =
    B.pstr_type Recursive
      [
        B.type_declaration ~name:{txt = "t"; loc} ~params:[] ~cstrs:[]
          ~kind:
            (Ptype_variant
               (List.map
                  (fun (c, _) ->
                    B.constructor_declaration ~name:{txt = c; loc}
                      ~args:(Pcstr_tuple []) ~res:None)
                  constructors))
          ~private_:Public ~manifest:None;
      ]
  in
  let all = B.elist (List.map (fun (c, _) -> constant c) constructors) in
  let to_string =
    B.pexp_function_cases
      (List.map
         (fun (c, v) ->
           B.case ~lhs:(constant_pattern c) ~guard:None ~rhs:(B.estring v))
         constructors)
  in
  let of_string =
    B.pexp_function_cases
      (List.map
         (fun (c, v) ->
           B.case ~lhs:(B.pstring v) ~guard:None
             ~rhs:[%expr Some [%e constant c]])
         constructors
      @ [B.case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr None]])
  in
  let items =
    [
      t;
      [%stri let all : t list = [%e all]];
      [%stri let to_string : t -> string = [%e to_string]];
      [%stri let of_string : string -> t option = [%e of_string]];
      [%stri let to_json : t -> Yojson.Safe.t = fun t -> `String (to_string t)];
      [%stri
        let of_json : Yojson.Safe.t -> t option = function
          | `String s -> of_string s
          | _ -> None];
    ]
  in
  B.pstr_module
    (B.module_binding
       ~name:{txt = Some module_name; loc}
       ~expr:(B.pmod_structure items))

(* An abstract id over any string: PostgREST sends uuids as strings, and
   checking their format is not the generator's job. *)
let id_module name =
  let signature =
    [%sig:
      type t

      val of_string : string -> t
      val to_string : t -> string
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_json : t -> Yojson.Safe.t
      val of_json : Yojson.Safe.t -> t option]
  in
  let structure =
    [%str
      type t = string

      let of_string s = s
      let to_string t = t
      let equal = String.equal
      let compare = String.compare
      let to_json t = `String t
      let of_json = function `String s -> Some s | _ -> None]
  in
  B.pstr_module
    (B.module_binding ~name:{txt = Some name; loc}
       ~expr:
         (B.pmod_constraint
            (B.pmod_structure structure)
            (B.pmty_signature signature)))

let phantom_type name =
  B.pstr_type Recursive
    [
      B.type_declaration ~name:{txt = name; loc} ~params:[] ~cstrs:[]
        ~kind:Ptype_abstract ~private_:Public ~manifest:None;
    ]

let structure (model : Model.t) =
  [%stri [@@@ocamlformat "disable"]]
  :: [%stri module Col = Postgrest.Col]
  :: List.map (fun (t : Model.table) -> phantom_type t.name) model.tables
  @ List.map lookup_module model.lookups
  @ List.map id_module model.id_modules
  @ List.map codec model.codecs
  @ List.map table_module model.tables

let header =
  {|(* Generated by postgrest-codegen from the database's catalog rows. Do not
   edit: change the schema, re-dump the catalog and regenerate.

   One module per table. A column's OCaml type follows its SQL type. A
   table with a uuid key gets a <Table>_id module typing it and the foreign
   keys to it, unless its `metadata:` line names one with id=; a lookup
   table gets a module generated from its values, unless it names one with
   ocaml=. Columns marked server_only are left out. text[] is carried as a
   JSON array, which PostgREST accepts and returns; a filter on it needs a
   hand-built {...} literal, not the column's encoding.

   style: ocamlformat is disabled because the generator owns the layout; no
   .mli because every value carries its full type annotation. *)
|}

let to_string model =
  let items =
    List.map
      (fun item -> Format.asprintf "%a" Pprintast.structure_item item)
      (structure model)
  in
  header ^ "\n" ^ String.concat "\n\n" items ^ "\n"

(* Test schema, declared the way the app's generated tables.ml would be.
   Mirrors fixtures/schema.sql. Phantom types first so the two tables can
   reference each other through relationships. *)

open Postgrest

type plants
type observations
type plant_status = Active | Archived

let plant_status_enum : plant_status Col.enum =
  {
    name = "plant_status";
    to_string = (function Active -> "active" | Archived -> "archived");
    of_string =
      (function
      | "active" -> Some Active
      | "archived" -> Some Archived
      | _ -> None);
  }

module Plants = struct
  type t = plants

  let table : t Col.table = Col.table "plants"

  let id : (t, string, [ `Mandatory ]) Col.t =
    { ty = Not_Null Str; name = "id" }

  let name : (t, string, [ `Mandatory ]) Col.t =
    { ty = Not_Null Str; name = "name" }

  let scientific_name : (t, string option, [ `Optional ]) Col.t =
    { ty = Nullable Str; name = "scientific_name" }

  (* NOT NULL with a default: optional on input, never null on output. *)
  let status : (t, plant_status, [ `Optional ]) Col.t =
    { ty = Not_Null (Enum plant_status_enum); name = "status" }

  let rating : (t, int option, [ `Optional ]) Col.t =
    { ty = Nullable Integer; name = "rating" }

  let height_cm : (t, float option, [ `Optional ]) Col.t =
    { ty = Nullable Numeric; name = "height_cm" }

  let is_native : (t, bool, [ `Optional ]) Col.t =
    { ty = Not_Null Bool; name = "is_native" }

  let created_at : (t, Ptime.t, [ `Optional ]) Col.t =
    { ty = Not_Null TimestampTz; name = "created_at" }

  let tags : (t, Yojson.Safe.t option, [ `Optional ]) Col.t =
    { ty = Nullable JSON; name = "tags" }

  let observations : (t, observations, 'a, 'a list) Col.relationship =
    { ty = Many; name = "observations" }
end

module Observations = struct
  type t = observations

  let table : t Col.table = Col.table "observations"

  let id : (t, string, [ `Mandatory ]) Col.t =
    { ty = Not_Null Str; name = "id" }

  let plant_id : (t, string, [ `Mandatory ]) Col.t =
    { ty = Not_Null Str; name = "plant_id" }

  let taken_at : (t, Ptime.t, [ `Mandatory ]) Col.t =
    { ty = Not_Null TimestampTz; name = "taken_at" }

  let note : (t, string option, [ `Optional ]) Col.t =
    { ty = Nullable Str; name = "note" }

  let lat : (t, float option, [ `Optional ]) Col.t =
    { ty = Nullable Numeric; name = "lat" }

  let lon : (t, float option, [ `Optional ]) Col.t =
    { ty = Nullable Numeric; name = "lon" }

  (* Embedding the parent through the foreign key: PostgREST names the
     embedded resource after the target table. *)
  let plant : (t, plants, 'a, 'a) Col.relationship =
    { ty = Exactly_one; name = "plants" }
end

(* Decoded records and the row specs that produce them. *)

type plant = {
  id : string;
  name : string;
  scientific_name : string option;
  status : plant_status;
  rating : int option;
  height_cm : float option;
  is_native : bool;
  created_at : Ptime.t;
  tags : Yojson.Safe.t option;
}

let plant_row : (plants, plant) Row.t =
  let open Row.Syntax in
  let+ id = Row.col Plants.id
  and+ name = Row.col Plants.name
  and+ scientific_name = Row.col Plants.scientific_name
  and+ status = Row.col Plants.status
  and+ rating = Row.col Plants.rating
  and+ height_cm = Row.col Plants.height_cm
  and+ is_native = Row.col Plants.is_native
  and+ created_at = Row.col Plants.created_at
  and+ tags = Row.col Plants.tags in
  {
    id;
    name;
    scientific_name;
    status;
    rating;
    height_cm;
    is_native;
    created_at;
    tags;
  }

type id_name = { id : string; name : string }

let id_name_row : (plants, id_name) Row.t =
  let open Row.Syntax in
  let+ id = Row.col Plants.id and+ name = Row.col Plants.name in
  { id; name }

type obs_id_note = { id : string; note : string option }

let obs_id_note_row : (observations, obs_id_note) Row.t =
  let open Row.Syntax in
  let+ id = Row.col Observations.id and+ note = Row.col Observations.note in
  { id; note }

type plant_with_obs = { id : string; name : string; obs : obs_id_note list }

let plant_with_obs_row : (plants, plant_with_obs) Row.t =
  let open Row.Syntax in
  let+ id = Row.col Plants.id
  and+ name = Row.col Plants.name
  and+ obs = Row.rel Plants.observations obs_id_note_row in
  { id; name; obs }

type obs_with_plant = { id : string; note : string option; plant : id_name }

let obs_with_plant_row : (observations, obs_with_plant) Row.t =
  let open Row.Syntax in
  let+ id = Row.col Observations.id
  and+ note = Row.col Observations.note
  and+ plant = Row.rel Observations.plant id_name_row in
  { id; note; plant }

(* Seed rows, as fixtures/seed.sql declares them. *)

let uuid n = Printf.sprintf "00000000-0000-0000-0000-%012d" n
let obs_uuid n = Printf.sprintf "10000000-0000-0000-0000-%012d" n

let ptime s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> invalid_arg ("bad timestamp in test: " ^ s)

let arnica =
  {
    id = uuid 1;
    name = "Arnica";
    scientific_name = Some "Arnica montana";
    status = Active;
    rating = Some 5;
    height_cm = Some 30.5;
    is_native = true;
    created_at = ptime "2026-03-01T10:00:00Z";
    tags =
      Some
        (`Assoc
           [
             ("color", `String "yellow"); ("alt", `List [ `Int 1500; `Int 2500 ]);
           ]);
  }

let gentiane =
  {
    id = uuid 2;
    name = "Gentiane jaune";
    scientific_name = Some "Gentiana lutea";
    status = Active;
    rating = None;
    height_cm = Some 120.;
    is_native = true;
    created_at = ptime "2026-04-15T08:30:00Z";
    tags = None;
  }

let inconnue =
  {
    id = uuid 3;
    name = "Plante, inconnue";
    scientific_name = Some "Mystery (sp.)";
    status = Archived;
    rating = Some 2;
    height_cm = None;
    is_native = false;
    created_at = ptime "2026-05-20T16:45:00Z";
    tags = Some (`List []);
  }

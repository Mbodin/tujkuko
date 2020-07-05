(** Manipulating measurement units. *)

(** The type of units.
   Note that it might include some metric prefix. *)
type t

(** Parse a unit from a string, retuning [None] in case no unit is recognised. *)
val parse : string -> t option

(** Print the unit. *)
val print : t -> string

(** The kind of the unit. *)
type kind =
  | Mass
  | Volume
  | Length
  | Time
  | Temperature

(** Get the kind ofa unit. *)
val get_kind : t -> kind

(** A unit system (e.g. metric or imperial) *)
type system

(** All the available systems.
 * The first one is usually the most preferable one. *)
val all_systems : system list

(** Given a unit system, normalises the unit. *)
val normalise : system -> float * t -> float * t


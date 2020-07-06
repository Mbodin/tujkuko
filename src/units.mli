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

(** Get the kind of a unit. *)
val get_kind : t -> kind

(** A unit system (e.g. metric) *)
type system

(** Each system is only made for a specific kind of units. *)
val get_system_kind : system -> kind

(** Given a system, returns its base unit. *)
val get_base_unit : system -> t

(** All the available systems, for each unit kinds.
   The first one is usually the most preferable one. *)
val all_systems : (kind, system) PMap.t

exception InvalidKind

(** Given a unit system, normalise a value.
   Raises [InvalidKind] if the system kind does not corresponds to the given unit kind. *)
val normalise : system -> float * t -> float * t

(* TODO: Import/Export functions *)


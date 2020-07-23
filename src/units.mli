(** Manipulating measurement units. *)

(** The type of units.
   Note that it might include some metric prefix. *)
type t

(** A value with a unit. *)
type value = float * t

(** Parse a unit from a string, retuning [None] in case no unit is recognised. *)
val parse : string -> t option

(** Print the unit. *)
val print : t -> string

(** The kind of the unit. *)
type kind =
  | Mass
  | Length
  | Volume
  | Time
  | Temperature

(** Get the kind of a unit. *)
val get_kind : t -> kind

(** A unit system (e.g. metric) *)
type system

(** Get a name for a system. *)
val system_name : system -> string

(** Inverse of [system_name]. *)
val parse_system : string -> system option

(** Each system is only made for a specific kind of units. *)
val get_system_kind : system -> kind

(** Given a system, return its base unit. *)
val get_base_unit : system -> t

(** Given a unit, return its associated system. *)
val get_system : t -> system

(** All the units defined in a system. *)
val system_units : system -> t list

(** All the available systems, for each unit kinds.
   The first one is usually the most preferable one. *)
val all_systems : (kind, system list) PMap.t

exception InvalidKind

(** Given a unit system, normalise a value.
   Raises [InvalidKind] if the system kind does not correspond to the given unit kind. *)
val normalise : system -> value -> value

(** Same as [normalise], but uses the value’s system. *)
val self_normalise : value -> value

(** Given a unit and a value, convert the value to this particular unit.
   Raises [InvalidKind] if the value and the unit do not correspond to the same unit kind. *)
val convert : t -> value -> float

(** Given a value and a unit, use a higher unit to represent the value.
   Return [None] if there is no higher avalaible units in the current unit system. *)
val increase_unit_value : value -> value option

(** Given a value and a unit, use a lower unit to represent the value.
   Return [None] if there is no lower avalaible units in the current unit system. *)
val decrease_unit_value : value -> value option

(* TODO: Import/Export functions *)


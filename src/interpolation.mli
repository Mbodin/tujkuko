(** Define how each measurments change the global factor. *)

(** The main type describing how a given unit interpolate with the global factor. *)
type t

(** The “correlation” of a constant measurment, that doesn’t change with the global factor. *)
val constant : t

(** A linear factor, where the global factor corresponds exactly to the given unit. *)
val linear : t

(** The default interpolation factor for a unit kind. *)
val default : Units.kind -> t

(** Functions to import and export correlations.
   The importation function may raise [Invalid_arg]. *)
val export : t -> Yojson.Safe.t
val import : Yojson.Safe.t -> t


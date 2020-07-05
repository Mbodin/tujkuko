
type kind =
  | Mass
  | Volume
  | Length
  | Time
  | Temperature

(** List of metric prefixes and their associated exponent. *)
let prefixes = [
    ("Y", 24) ;
    ("Z", 21) ;
    ("E", 18) ;
    ("P", 15) ;
    ("T", 12) ;
    ("G", 9) ;
    ("M", 6) ;
    ("k", 3) ;
    ("h", 2) ;
    ("da", 1) ;
    ("", 0) ;
    ("d", -1) ;
    ("c", -2) ;
    ("m", -3) ;
    ("μ", -6) ;
    ("n", -9) ;
    ("p", -12) ;
    ("f", -15) ;
    ("a", -18) ;
    ("z", -21) ;
    ("y", -24)
  ]

type t = {
    unit_kind : kind (** The kind of the unit. *) ;
    base_unit : string (** The notation for the base unit (e.g. "g" for grams. *) ;
    unit_power : int (** The current order of magnitude. *) (* TODO: Maybe not: see how the unit system is defined. *)
  }

let get_kind u = u.unit_kind

(** A unit system describes which base units should be used and when,
   which prefixes should be used, and how to convert them. *)
type unit_system
(* TODO: A list of unit names and value: each of the elements in this list state how many
   of the next one is needed to get the current one (this list is generated from the prefixes
   for metric units, and partially generated from it for the common abbreviation one.
   Furthermore, they should store how the base unit relates to the related main metric unit. *)

(** A system just associates for each kind a unit system. *)
type system = (kind, unit_system) PMap.t


let all_systems = []
  (* TODO: Metric with common abbreviations (e.g. megatons instead of gigagrams),
     metric (only using multiples of 3 for prefixes),
     common devices (spoons, cups, etc.), and imperial *)


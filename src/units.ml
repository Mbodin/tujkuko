
open Libutils

type kind =
  | Mass
  | Volume
  | Length
  | Time
  | Temperature

exception InvalidKind

(** A unit system describes which base units should be used and when,
   which prefixes should be used, and how to convert them. *)
type system = {
    system_id : Id.t (** A unique identifier, to ease comparisons. *) ;
    system_kind : kind (** The kind of the unit system. *) ;
    base_value : float
      (** How the base value relates to the corresponding base value of the metric system. *) ;
    base_notation : string (** The notation of the base unit of this unit system. *) ;
    higher_units : (string * float) list
      (** A list of pairs [(name, value)].
         The value represents the number of the previous unit that this next unit is worth of. *) ;
    lower_units : (float * string) list
      (** A list of pairs [(value, name)].
         The value represents the number of the next unit that this previous unit is worth of. *)
  }

(** Generate new identifiers for systems. *)
let new_system_id = Id.new_id_function ()

type t = {
    unit_notation : string (** The notation of the unit. *) ;
    unit_system : system (** The system associated to the unit. *) ;
    (** The [higher_units] and [lower_units] lists are repeated, but centered around
       the current unit, making it easier to navigate. *)
    unit_higher : (string * float) list ;
    unit_lower : (float * string) list ;
    big_unit : bool (** State whether the base of the system is smaller than the current unit. *)
  }

let get_system_kind s = s.system_kind
let get_kind u = get_system_kind u.unit_system

let print u = u.unit_notation

let get_base_unit s = {
    unit_notation = s.base_notation ;
    unit_system = s ;
    unit_higher = s.higher_units ;
    unit_lower = s.lower_units ;
    big_unit = false
  }

(** Given a value and a unit, use a higher unit to represent the value. *)
let increase_unit_value (v, u) =
  match u.unit_higher with
  | [] -> invalid_arg ("increase_unit_value: " ^ print u) (** No bigger unit available. *)
  | (b, q) :: l ->
    (v /. q, {
      unit_notation = b ;
      unit_system = u.unit_system ;
      unit_higher = l ;
      unit_lower = (q, u.unit_notation) :: u.unit_lower ;
      big_unit = u.big_unit || u.unit_notation = u.unit_system.base_notation
    })

(** Given a value and a unit, use a lower unit to represent the value. *)
let decrease_unit_value (v, u) =
  match u.unit_lower with
  | [] -> invalid_arg ("decrease_unit_value: " ^ print u) (** No bigger unit available. *)
  | (q, b) :: l ->
    (v *. q, {
      unit_notation = b ;
      unit_system = u.unit_system ;
      unit_higher = (u.unit_notation, q) :: u.unit_higher ;
      unit_lower = l ;
      big_unit = if u.big_unit then b <> u.unit_system.base_notation else false
    })

(** Given a unit, converts it to a value into the base unit of the system. *)
let rec convert_to_base (v, u) =
  if u.unit_notation = u.unit_system.base_notation then (v, u)
  else if u.big_unit then convert_to_base (decrease_unit_value (v, u))
  else convert_to_base (increase_unit_value (v, u))

(** Normalise a pair value/unit within its own system. *)
let rec simple_normalise (v, u) =
  if v < 1. && u.unit_lower <> [] then
    simple_normalise (decrease_unit_value (v, u))
  else
    match u.unit_higher with
    | (_, q) :: _ when v > q ->
      simple_normalise (increase_unit_value (v, u))
    | _ -> (v, u)

let normalise s (v, u) =
  (** First, convert to the same unit system. *)
  let (v, u) =
    if s.system_id = u.unit_system.system_id then (v, u)
    else
      let (v, u) = convert_to_base (v, u) in
      (v *. s.base_value /. u.unit_system.base_value, get_base_unit s) in
  simple_normalise (v, u)

(** List of metric prefixes and their associated exponent. *)
let si_prefixes = [
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

let all_systems =
  ignore (new_system_id, si_prefixes) ;
  PMap.empty
  (* TODO: Metric with common abbreviations (e.g. megatons instead of gigagrams,
     and only using multiples of 3 for prefixes),
     metric, common devices (spoons, cups, etc.), etc. *)

let parse str =
  let str = String.trim str in
  PMap.fold (fun s -> function
    | Some r -> Some r
    | None ->
      if str = s.base_notation then Some (get_base_unit s)
      else
        let rec aux current higher = function
          | [] -> None
          | (q, b) :: lower ->
            let higher = (current, q) :: higher in
            if str = b then
              Some {
                  unit_notation = b ;
                  unit_system = s ;
                  unit_higher = higher ;
                  unit_lower = lower ;
                  big_unit = false
                }
            else aux b higher lower in
        match aux s.base_notation s.higher_units s.lower_units with
        | Some r -> Some r
        | None ->
          let rec aux current lower = function
            | [] -> None
            | (b, q) :: higher ->
              let lower = (q, current) :: lower in
              if str = b then
                Some {
                  unit_notation = b ;
                  unit_system = s ;
                  unit_higher = higher ;
                  unit_lower = lower ;
                  big_unit = true
                  }
              else aux b lower higher in
          aux s.base_notation s.lower_units s.higher_units) all_systems None

(* TODO: Unit tests. *)


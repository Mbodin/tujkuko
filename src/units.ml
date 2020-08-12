
open Libutils

(** * Type declarations *)

type kind =
  | Mass
  | Length
  | Volume
  | Time
  | Temperature

exception InvalidKind


(** A type for unit notations. *)
type notation = {
    main_notation : string (** The main notation *) ;
    alternative_notations : string list (** Alternative notations, mainly used for parsing. *)
  }

(** A unit system describes which base units should be used and when,
   which prefixes should be used, and how to convert them. *)
type system = {
    system_id : string
      (** An identifier, to ease comparisons.
         It must be unique for a given [system_kind]. *) ;
    system_kind : kind (** The kind of the unit system. *) ;
    base_value : float
      (** How the base value relates to the corresponding base value of the metric system. *) ;
    base_shift : float (** An eventual shift in the zero value, expressed in the metric system. *) ;
    base_notation : notation (** The notation of the base unit of this unit system. *) ;
    higher_units : (notation * float) list
      (** A list of pairs [(notation, value)].
         The value represents the number of the previous unit that this next unit is worth of. *) ;
    lower_units : (float * notation) list
      (** A list of pairs [(value, notation)].
         The value represents the number of the next unit that this previous unit is worth of. *)
  }

type t = {
    unit_notation : notation (** The notation of the unit. *) ;
    unit_system : system (** The system associated to the unit. *) ;
    (** The [higher_units] and [lower_units] lists are repeated, but centered around
       the current unit, making it easier to navigate. *)
    unit_higher : (notation * float) list ;
    unit_lower : (float * notation) list ;
    big_unit : bool (** State whether the base of the system is smaller than the current unit. *)
  }

(** * Basic manipulations *)

type value = float * t

let get_system_kind s = s.system_kind
let get_system u = u.unit_system
let get_kind u = get_system_kind (get_system u)
let print u = u.unit_notation.main_notation

let get_base_unit s = {
    unit_notation = s.base_notation ;
    unit_system = s ;
    unit_higher = s.higher_units ;
    unit_lower = s.lower_units ;
    big_unit = false
  }

let kind_name = function
  | Mass -> "mass"
  | Volume -> "volume"
  | Length -> "length"
  | Time -> "time"
  | Temperature -> "temperature"

let system_name s =
  Printf.sprintf "%s-%s" s.system_id (kind_name s.system_kind)

(** * Conversions *)

let increase_unit_value (v, u) =
  match u.unit_higher with
  | [] -> None (** No bigger unit available. *)
  | (b, q) :: l ->
    Some (v /. q, {
      unit_notation = b ;
      unit_system = u.unit_system ;
      unit_higher = l ;
      unit_lower = (q, u.unit_notation) :: u.unit_lower ;
      big_unit = u.big_unit || u.unit_notation = u.unit_system.base_notation
    })

let decrease_unit_value (v, u) =
  match u.unit_lower with
  | [] -> None (** No bigger unit available. *)
  | (q, b) :: l ->
    Some (v *. q, {
      unit_notation = b ;
      unit_system = u.unit_system ;
      unit_higher = (u.unit_notation, q) :: u.unit_higher ;
      unit_lower = l ;
      big_unit = if u.big_unit then b <> u.unit_system.base_notation else false
    })

let system_units s =
  let u = get_base_unit s in
  let rec aux acc f u =
    match f (1., u) with
    | None -> acc
    | Some (_, u) -> aux (u :: acc) f u in
  let ll = List.rev (aux [] decrease_unit_value u) in
  aux (u :: ll) increase_unit_value u

(** Given a unit, converts it to a value into the base unit of the system. *)
let rec convert_to_base (v, u) =
  if u.unit_notation = u.unit_system.base_notation then (v, u)
  else
    let f = if u.big_unit then decrease_unit_value else increase_unit_value in
    let (v, u) = Utils.assert_option __LOC__ (f (v, u)) in
    convert_to_base (v, u)

let self_normalise (v, u) =
  let rec aux (v, u) =
    if v < 1. then
      match decrease_unit_value (v, u) with
      | Some (v, u) -> aux (v, u)
      | None -> (v, u)
    else
      match u.unit_higher with
      | (_, q) :: _ when v > q ->
        let (v, u) = Utils.assert_option __LOC__ (increase_unit_value (v, u)) in
        aux (v, u)
      | _ -> (v, u) in
  if v = 0. then
    (0., get_base_unit (get_system u))
  else aux (v, u)

(** Convert a measure to the base metric value. *)
let to_metric (v, u) =
  let (v, u) = convert_to_base (v, u) in
  u.unit_system.base_shift +. v *. u.unit_system.base_value

(** Convert a value to a (non-normalised) value to the provided unit system. *)
let convert_to_system s (v, u) =
  if s.system_id = u.unit_system.system_id then (v, u)
  else
    (** First, moving to metric. *)
    let v = to_metric (v, u) in
    (** Then converting to the new unit *)
    ((v -. s.base_shift) /. s.base_value, get_base_unit s)

let normalise s (v, u) =
  let (v, u) = convert_to_system s (v, u) in
  self_normalise (v, u)

let convert u' (v, u) =
  let (v, u) = convert_to_system u'.unit_system (v, u) in
  if print u = print u' then v
  else
    let shift =
      if List.exists (fun (n, _) -> n.main_notation = print u') u.unit_higher then
        increase_unit_value
      else decrease_unit_value in
    let rec aux (v, u) =
      match shift (v, u) with
      | None -> assert false
      | Some (v, u) ->
        if print u = print u' then v
        else aux (v, u) in
    aux (v, u)

(** * Functions to create units *)

(** ** Exponential *)

(** The following functions are mostly based on exponential scales. *)

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

(** Given a list of base unit notations (the first being the prefered one) and a prefix,
   create a notation for this unit with this prefix. *)
let notation_base = function
  | [] -> assert false
  | base :: others -> fun pr -> {
      main_notation = pr ^ base ;
      alternative_notations = List.map (fun u -> pr ^ u) others
    }

(** A smart constructor for when we just need to provide a list of notations. *)
let direct_notation l = notation_base l ""

(** Inverse [direct_notation]. *)
let inverse_direct_notation n =
  n.main_notation :: n.alternative_notations

let%test_unit _ =
  let test l = assert (inverse_direct_notation (direct_notation l) = l) in
  test ["m"] ;
  test ["m³" ; "m^3"] ;
  test ["a" ; "b" ; "c"]

(** Given a list of notations and associated exponents,
   this function return a pair of lists, one for the higher units, and
   one for the lower ones, with their associated values. *)
let create_base10_unit notexp =
  let (low, high) = List.partition (fun (_, i) -> i <= 0) notexp in
  let low = List.sort (fun (_, i1) (_, i2) -> - compare i1 i2) low in
  let low =
    let rec aux current acc = function
      | [] -> List.rev acc
      | (notation, exponent) :: low ->
        let value = Float.pow 10. (Float.of_int (current - exponent)) in
        aux exponent ((value, notation) :: acc) low in
    aux 0 [] low in
  let high = List.sort (fun (_, i1) (_, i2) -> compare i1 i2) high in
  let high =
    let rec aux current acc = function
      | [] -> List.rev acc
      | (notation, exponent) :: high ->
        let value = Float.pow 10. (Float.of_int (exponent - current)) in
        aux exponent ((notation, value) :: acc) high in
    aux 0 [] high in
  (low, high)

(** Create a list of units, given a base unit name and the SI prefixes
   (with their associated exponent) to use.
   This function return a pair of lists, one for the higher units, and
   one for the lower ones, with their associated values. *)
let create_si_unit base prefixes =
  create_base10_unit (List.map (fun (b, i) -> (notation_base base b, i)) prefixes)

(** A smart constructor for metric-like units. *)
let create_metric ?(remove0=true) kind name base prefixes =
  let prefixes =
    if remove0 then
      List.filter (fun (_, i) -> i <> 0) prefixes
    else prefixes in
  let (lower, higher) = create_si_unit base prefixes in {
    system_id = name ;
    system_kind = kind ;
    base_notation = direct_notation base ;
    base_value = 1. ;
    base_shift = 0. ;
    higher_units = higher ;
    lower_units = lower
  }

(** Similar to [create_metric], but this time is given a list of pairs:
  - a unit, associated to some prefixes,
  - its list of associated prefixes and their associated values. *)
let create_discontinued ?(remove0=false) kind name base value l =
  let l =
    List.concat (List.map (fun (base, prefixes) ->
      List.map (fun (p, e) -> (notation_base base p, e)) prefixes) l) in
  let l =
    if remove0 then
      List.filter (fun (_, i) -> i <> 0) l
    else l in
  let (lower, higher) = create_base10_unit l in {
    system_id = name ;
    system_kind = kind ;
    base_notation = base ;
    base_value = value ;
    base_shift = 0. ;
    higher_units = higher ;
    lower_units = lower
  }

(** ** Absolutes *)

(** The above functions are mostly based on exponential values (and in particularly, base 10).
   The following functions are based on absolute values. *)

(** Given a list of pairs of notation and absolute values (in any coherent unit), generate the
   lower and higher lists.
   It takes as argument the base value used as the reference unit.
   This is thus similar to [create_base10_unit], but with absolute units. *)
(* TODO: Factorise with [create_base10_unit]. *)
let create_absolute base_value notval =
  let (low, high) = List.partition (fun (_, v) -> v <= base_value) notval in
  let low = List.sort (fun (_, i1) (_, i2) -> - compare i1 i2) low in
  let low =
    let rec aux current acc = function
      | [] -> List.rev acc
      | (notation, value) :: low ->
        let dvalue = current /. value in
        aux value ((dvalue, notation) :: acc) low in
    aux base_value [] low in
  let high = List.sort (fun (_, i1) (_, i2) -> compare i1 i2) high in
  let high =
    let rec aux current acc = function
      | [] -> List.rev acc
      | (notation, value) :: high ->
        let dvalue = value /. current in
        aux value ((notation, dvalue) :: acc) high in
    aux base_value [] high in
  (low, high)

(** Given a list of units and their absolute values, as well as a kind, a name, and a base value,
   create a unit system with all these units. *)
(* TODO: Factorise [create_metric], [create_discontinued], [create_from_absolute_list],
   and [create_from_absolutes]. *)
let create_from_absolute_list kind name base base_value notval =
  let (lower, higher) = create_absolute base_value notval in {
    system_id = name ;
    system_kind = kind ;
    base_notation = direct_notation base ;
    base_value = base_value ;
    base_shift = 0. ;
    higher_units = higher ;
    lower_units = lower
  }

(** Similarly to [create_from_absolute_list], but the base unit is simply taken from the unit
   associated to [1.] in the list.
   This means that the list is provided relative to the base unit of the system.
   An additional value is provided to explain what the actual metric value of this base unit is. *)
let create_from_absolutes kind name value notval =
  match List.find_opt (fun (_, v) -> v = 1.) notval with
  | None -> assert false
  | Some (u, _) ->
    let notval = List.filter (fun (_, v) -> v <> 1.) notval in
    let u = inverse_direct_notation u in
    let s = create_from_absolute_list kind name u 1. notval in
    { s with base_value = value }

(** * System definitions. *)

(** Mass units. *)

let metric_mass =
  let s = create_metric Mass "metric" ["g"] (List.map (fun (p, e) -> (p, e - 3)) si_prefixes) in
  { s with base_notation = notation_base ["g"] "k" }

let usual_mass =
  let si_prefixes = List.filter (fun (_, e) -> e mod 3 = 0) si_prefixes in
  let small_prefixes =
    let prefixes = List.map (fun (p, e) -> (p, e - 3)) si_prefixes in
    List.filter (fun (_, e) -> e <= 0) prefixes in
  let large_prefixes =
    let prefixes = List.map (fun (p, e) -> (p, e + 3)) si_prefixes in
    List.filter (fun (_, e) -> e >= 3 && e <= 18) prefixes in
  let s =
    create_discontinued ~remove0:true Mass "usual" (notation_base ["g"] "k") 1. [
        (["g"], small_prefixes) ;
        (["t"; "T"; "mT"], large_prefixes)
      ] in
  { s with higher_units =
            s.higher_units @ [
                (direct_notation ["M_L"; "ML"], 73.42 (* Et *)) (** Lunar mass *) ;
                (direct_notation ["M_⊕"; "M⊕"], 81.3) (** Earth mass *) ;
                (direct_notation ["M_J"; "MJ"], 317.82838) (** Jovian mass *) ;
                (direct_notation ["M_☉"; "M☉"], 1_047.348644) (** Solar mass *) ;
              ] }

(** Length units. *)

let metric_length =
  create_metric Length "metric" ["m"] si_prefixes

let usual_length =
  let prefixes = List.filter (fun (_, e) -> e <= 3 && (e mod 3 = 0 || e = -2)) si_prefixes in
  let s = create_metric Length "usual" ["m"] prefixes in
  { s with higher_units =
            s.higher_units @ [
                (direct_notation ["R_☉"; "R☉"], 695_700. (* km *)) (** Solar radius *) ;
                (direct_notation ["AU"], 215.032155671) (** Astronomical unit *) ;
                (direct_notation ["ly"], 63_241.) (** Light year *) ;
              ] }

let imperial_length =
  create_from_absolute_list Length "imperial" ["yd"] 0.9144 (** yard *) [
      (direct_notation ["th"], 0.000_0254) (** thou *) ;
      (direct_notation ["″"; "in"; "\""], 0.0254) (** inch *) ;
      (direct_notation ["′"; "ft"; "'"], 0.3048) (** foot *) ;
      (direct_notation ["ch"], 20.1168) (** chain *) ;
      (direct_notation ["fur"], 201.168) (** furlong *) ;
      (direct_notation ["mi"], 1_609.344) (** mile *) ;
      (direct_notation ["lea"], 4_828.032) (** league *) ;
    ]

(** Volume units. *)

let metric_volume =
  let m3 = ["m³"; "m^3"] in
  let prefixes = List.map (fun (p, e) -> (p, 3 * e)) si_prefixes in
  create_discontinued Volume "metric" (direct_notation m3) 1. [
      (m3, List.filter (fun (_, e) -> e <> 0) prefixes)
    ]

let liter_volume =
  let l = ["ℓ"; "l"; "L"] in
  let liter_prefixes =
    let prefixes = List.map (fun (p, e) -> (p, e - 3)) si_prefixes in
    List.filter (fun (_, e) -> e mod 3 = 0) prefixes in
  create_discontinued Volume "liter" (notation_base l "k") 1. [
      (l, List.filter (fun (_, e) -> e <> 0) liter_prefixes)
    ]

(** Common cooking devices *)
let devices_volume =
  (** Each time there is a conflict, the metric version is used: the tablespoon here means
     the metric tablespoon. *)
  create_from_absolute_list Volume "devices" ["tbsp"; "tbs"] 0.000_015 (** tablespoon *) [
      (direct_notation ["teasp" ; "tsp"], 0.000_005) (** teaspoon *) ;
      (direct_notation ["dstspn"], 0.000_010) (** dessert spoon *) ;
      (direct_notation ["cup"; "c"], 0.000_250) (** cup *) ;
      (direct_notation ["TEU"], 33.2) (** twenty-foot equivalent unit *) ;
    ]

let imperial_volume =
  create_from_absolutes Volume "imperial" 0.000_028_4130625 [
      (direct_notation ["fl oz"; "℥"; "fl ℥"; "f℥"; "ƒ ℥"], 1.) (** fluid ounce *) ;
      (direct_notation ["gi"], 5.) (** gill *) ;
      (direct_notation ["pt"; "O"; "p"], 20.) (** pint *) ;
      (direct_notation ["qt"], 40.) (** quart *) ;
      (direct_notation ["gal"; "C"], 160.) (** gallon *) ;
    ]

(** Time units. *)

let metric_time =
  create_metric Time "metric" ["s"] si_prefixes

let usual_time =
  let si_prefixes = List.filter (fun (_, e) -> e mod 3 = 0) si_prefixes in
  let small_prefixes =
    List.filter (fun (_, e) -> e <= 0) si_prefixes in
  let large_prefixes =
    List.filter (fun (_, e) -> e >= 3) si_prefixes in
  let s =
    create_discontinued ~remove0:true Time "usual" (direct_notation ["s"]) 1. [
        (["s"], small_prefixes) ;
        (["a"], large_prefixes)
      ] in
  { s with higher_units = [
          (direct_notation ["min"], 60.) ;
          (direct_notation ["h"], 60.) ;
          (direct_notation ["hr"], 1.) ;
          (direct_notation ["d"], 24.) ;
          (direct_notation ["a"], 365.25)
        ] @ s.higher_units }

(** Temperature units. *)

let kelvin =
  create_metric Temperature "metric" ["K"] si_prefixes

let celsius = {
    system_id = "celsius" ;
    system_kind = Temperature ;
    base_notation = direct_notation ["°C"] ;
    base_value = 1. ;
    base_shift = 273.16 ;
    higher_units = [] ;
    lower_units = []
  }

let fahrenheit = {
    system_id = "fahrenheit" ;
    system_kind = Temperature ;
    base_notation = direct_notation ["°F"] ;
    base_value = 0.55555555555555558 ;
    base_shift = 255.372222 ;
    higher_units = [] ;
    lower_units = []
  }

let french_thermostat = {
    system_id = "fr-thermostat" ;
    system_kind = Temperature ;
    base_notation = direct_notation ["Thermostat"] ;
    base_value = 27.77777777777778 ;
    base_shift = 300.937777778 ;
    higher_units = [] ;
    lower_units = []
  }

let german_thermostat = {
    system_id = "de-Stufe" ;
    system_kind = Temperature ;
    base_notation = direct_notation ["Stufe"] ;
    base_value = 25. ;
    base_shift = 398.16 ;
    higher_units = [] ;
    lower_units = []
  }

let gas_mark = {
    system_id = "uk-gas-mark" ;
    system_kind = Temperature ;
    base_notation = direct_notation ["Gas Mark"] ;
    base_value = 13.88888888888889 ;
    base_shift = 383.16 ;
    higher_units = [] ;
    lower_units = []
  }

let all_systems =
  let m = PMap.empty in
  let m =
    PMap.add Mass [
        usual_mass ;
        metric_mass
      ] m in
  let m =
    PMap.add Length [
        usual_length ;
        metric_length ;
        imperial_length
      ] m in
  let m =
    PMap.add Volume [
        liter_volume ;
        metric_volume ;
        devices_volume ;
        imperial_volume
      ] m in
  let m = PMap.add Time [
        usual_time ;
        metric_time
      ] m in
  let m =
    PMap.add Temperature [
        celsius ;
        fahrenheit ;
        kelvin ;
        french_thermostat ;
        german_thermostat ;
        gas_mark
      ] m in
  m

(** * Tests *)

let%test _ =
  (** System names are unique. *)
  PMap.fold (fun l b ->
    b &&
      let l = List.mapi (fun i s -> (i, system_name s)) l in
      List.for_all (fun (i1, u1) ->
        List.for_all (fun (i2, u2) ->
          let b = i1 = i2 || u1 <> u2 in
          if not b then Printf.printf "System defined twice: %s (indices %d and %d).\n%!" u1 i1 i2 ;
          b) l) l) all_systems true

let%test_unit _ =
  (** Just printing all defined units. *)
  PMap.iter (fun kind l ->
    Printf.printf "%s:\n" (kind_name kind) ;
    List.iter (fun s ->
      Printf.printf "\t%s: %s.\n%!" (system_name s)
        (String.concat ", " (List.map print (system_units s)))) l) all_systems

let%test_unit _ =
  (** State that two values are close enough one to the other. *)
  let close_enough u v1 v2 =
    let b = 0.999 *. v1 <= v2 && v2 <= 1.001 *. v1 in
    if not b then Printf.printf "Unit %s defined twice, but %F ≠ %F.\n%!" u v1 v2 ;
    b in
  (** If two units have the same name, then they convert one to another. *)
  ignore (PMap.foldi (fun kind l units ->
    List.fold_left (fun units s ->
      List.fold_left (fun units u ->
          assert (
            let b = kind = get_kind u in
            if not b then
              Printf.printf "System kind clash for %s (%s vs %s).\n%!"
                (print u) (kind_name kind) (kind_name (get_kind u)) ;
            b) ;
          if PMap.mem (print u) units then (
            (* There is a name conflict: let us check that these units convert
               well one to the other.  *)
            let (u', s') = PMap.find (print u) units in
            assert (get_kind u = get_kind u')
              (* Their kinds are the same. *) ;
            assert (
              let b = system_name s <> system_name s' in
              if not b then
                Printf.printf "System name clash: %s in %s.\n%!" (print u) (system_name s) ;
              b) (* Two units in the same system never clash. *) ;
            let v1 = to_metric (1., u') in
            let v2 = to_metric (1., u') in
            assert (close_enough (print u) v1 v2) ;
            units
          ) else PMap.add (print u) (u, s) units)
        units (system_units s)) units l) all_systems PMap.empty)

(** * Parsing *)

let parse_system name =
  PMap.fold (fun l -> function
    | Some s -> Some s
    | None ->
      List.fold_left (function
        | Some s -> fun _ -> Some s
        | None -> fun s -> if system_name s = name then Some s else None) None l) all_systems None

(** Given a notation and a string, return a boolean is the string matches the notation. *)
let recognise_notation notation str =
  List.mem str (notation.main_notation :: notation.alternative_notations)

let parse_list str =
  let str = String.trim str in
  let l =
    PMap.fold (fun l others ->
      List.fold_left (fun others s ->
        let others =
          if recognise_notation s.base_notation str then
            (get_base_unit s) :: others
          else others in
        let rec aux others current higher = function
          | [] -> others
          | (q, b) :: lower ->
            let higher = (current, q) :: higher in
            let others =
              if recognise_notation b str then {
                  unit_notation = b ;
                  unit_system = s ;
                  unit_higher = higher ;
                  unit_lower = lower ;
                  big_unit = false
                } :: others
              else others in
            aux others b higher lower in
        let others = aux others s.base_notation s.higher_units s.lower_units in
        let rec aux others current lower = function
          | [] -> others
          | (b, q) :: higher ->
            let lower = (q, current) :: lower in
            let others =
              if recognise_notation b str then {
                  unit_notation = b ;
                  unit_system = s ;
                  unit_higher = higher ;
                  unit_lower = lower ;
                  big_unit = true
                } :: others
              else others in
            aux others b lower higher in
          aux others s.base_notation s.lower_units s.higher_units) others l) all_systems [] in
  (** Now that we have the list of units that get recognised, we place the exact matches first. *)
  let (exact, alternative) = List.partition (fun u -> u.unit_notation.main_notation = str) l in
  exact @ alternative

let parse str =
  match parse_list str with
  | [] -> None
  | u :: _ ->
    (** Because [parse_list] already sorts them, we only need to check the first item. *)
    if u.unit_notation.main_notation = str then Some u else None


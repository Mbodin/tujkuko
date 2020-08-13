
type t =
  | Constant (** This measurment is uncorrelated with the global factor. *)
  | Exponent of int (** The measurment grows with the [n]th power of the global factor.
                      We assume [n > 0]. *)
  | Root of int (** The global factor grows with the [n]th power of the measurment.
                   Again, [n > 0]. *)

let constant = Constant

let linear = Exponent 1

let default = function
  | Units.Mass -> linear
  | Units.Length -> Root 2 (** If one is measuring lengths, it is usually on a surface. *)
  | Units.Volume -> linear
  | Units.Time -> constant
  | Units.Temperature -> constant

let export = function
  | Constant -> `String "const"
  | Exponent i ->
    `Assoc [
        ("correlation", `String "exp") ;
        ("value", `Int i)
      ]
  | Root i ->
    `Assoc [
        ("correlation", `String "root") ;
        ("value", `Int i)
      ]

let get_field ctx fld l : Yojson.Safe.t =
  try List.assoc fld l
  with Not_found -> failwith (ctx ^ ", get_field: Missing field " ^ fld)

let get_field_string ctx fld l =
  match get_field (ctx ^ ", get_field_string") fld l with
  | `String str -> str
  | _ -> failwith (ctx ^ ", get_field_string: not a string")

let get_field_int ctx fld l =
  match get_field (ctx ^ ", get_field_int") fld l with
  | `Int i -> i
  | _ -> failwith (ctx ^ ", get_field_int: not an integer")

let import = function
  | `String "const" -> Constant
  | `String str -> failwith ("Interpolation.import: unknown string `" ^ str ^ "'")
  | `Assoc l ->
    let value = get_field_int "Interpolation.import" "value" l in
    (match get_field_string "Interpolation.import" "correlation" l with
     | "exp" -> Exponent value
     | "root" -> Root value
     | str -> failwith ("Interpolation.import: unkown kind `" ^ str ^ "'"))
  | _ -> failwith ("Interpolation.import: expecting a string or an object")


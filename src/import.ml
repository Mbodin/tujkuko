
let recode ?encoding src =
  let rec loop d e = match Uutf.decode d with
  | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
  | `Await -> assert false in
  let nln = Some (`NLF (Uchar.of_char '\n')) in
  let d = Uutf.decoder ?nln ?encoding (`String src) in
  let dst = Buffer.create (String.length src) in
  let e = Uutf.encoder `UTF_8 (`Buffer dst) in
  loop d e ;
  Buffer.contents dst

let get_field ctx fld l : Yojson.Safe.t =
  try List.assoc fld l
  with Not_found -> failwith (ctx ^ ", get_field: Missing field " ^ fld)

let get_field_string ctx fld l =
  match get_field (ctx ^ ", get_field_string") fld l with
  | `String str -> str
  | _ -> failwith (ctx ^ ", get_field_string: not a string")


let get_field_float ctx fld l =
  match get_field (ctx ^ ", get_field_float") fld l with
  | `Int i -> float_of_int i
  | `Float f -> f
  | _ -> failwith (ctx ^ ", get_field_float: not a number")

let get_field_bool ctx fld l =
  match get_field (ctx ^ ", get_field_bool") fld l with
  | `Bool b -> b
  | _ -> failwith (ctx ^ ", get_field_bool: not a boolean")

let read_unit l =
  if get_field_bool "read_unit" "unit" l then
    Some {
        Recipe.munit_notation = get_field_string "read_unit" "notation" l ;
        Recipe.munit_metric = get_field_bool "read_unit" "metric" l
      }
  else None

let read_item = function
  | `String str -> Recipe.Sentence str
  | `Assoc l ->
    (match get_field_string "read_item" "kind" l with
     | "unit" ->
       let get_field_float f = get_field_float "read_item" f l in
       Recipe.Unit (get_field_float "min", get_field_float "max", read_unit l)
     | str -> failwith ("read_item: unknown kind: " ^ str))
  | _ -> failwith "read_item: unexpected argument"

let read_step = function
  | `List l -> List.map read_item l
  | _ -> failwith "read_step: expecting a list"

let read_description = function
  | `Assoc l ->
    List.fold_left (fun m (lg, s) -> PMap.add lg (read_step s) m) PMap.empty l
  | _ -> failwith "read_description: expecting an object"

let read_hints = function
  | `Assoc l ->
    List.fold_left (fun m (lg, h) ->
      match h with
      | `List l ->
        PMap.add lg (List.map read_step l) m
      | _ -> failwith "read_hints: expecting a list") PMap.empty l
  | _ -> failwith "read_hints: expecting an object"

let from_json fileContent =
  let fileContent = recode fileContent in
  let rec read = function
    | `Assoc l ->
      let info = {
          Recipe.picture =
            Option.map (function
              | `String a -> a
              | _ -> failwith "read: expected a string") (List.assoc_opt "picture" l) ;
          description = read_description (get_field "from_json, read" "description" l) ;
          hints = read_hints (get_field "from_json, read" "hints" l) ;
        } in
      (info, read_t (get_field "read" "next" l))
    | _ -> failwith "read: not an object"
  and read_t = function
  | `String "end" -> Recipe.End
  | `List l -> Recipe.Step (List.map read l)
  (* [`Assoc l], with each key representing a parameter (quantity of ingredients chosen, ingredient chosen, etc.) *)
  | _ -> failwith "from_json, read_t: unexpected argument" in
  match Yojson.Safe.from_string fileContent with
  | `Assoc l -> read_t (get_field "from_json" "recipes" l)
  | _ -> failwith "from_json: not an object"


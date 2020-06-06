
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

let get_field ctx fld l =
  try List.assoc fld l
  with Not_found -> failwith (ctx ^ ", get_field: Missing field " ^ fld)

let read_item = function
  | `String str -> Recipe.Sentence str
  | `Assoc l ->
    let get_field f = get_field  "read_item" l in
    (match get_field "kind" with
     | `String "unit" ->
       let get_int f =
         match get_field f with
         | `Int i -> i
         | _ -> failwith "read_item, get_int: expecting an integer" in
       Recipe.Unit (get_int "min", get_int "max", {
           munit_notation =
             (match get_field "notation" with
              | `String u -> u
              | _ -> failwith "get_item: notation not a string") ;
           munit_metric =
             (match get_field "metric" with
              | `String "yes" -> true
              | `String "no" -> false
              | `String m -> failwith ("get_item: unknown metric value: " ^ m)
              | _ -> failwith "get_item: metric not a string") ;
         })
     | `String str -> failwith ("read_item: unknown kind: " ^ str)
     | _ -> failwith "read_item: kind not a string")
  | _ -> failwith "read_item: unexpected argument"

let read_step = function
  | `List l -> List.map read_item l
  | _ -> failwith "read_step: expecting a list"

let read_description = function
  | `Assoc l ->
    List.fold_left (fun m (lg, s) -> PMap.add lg (read_step s) m) PMap.empty l
  | _ -> failwith "read_description: expecting an object"

let from_json =
  let rec read = function
    | `Assoc l ->
      let info = {
          Recipe.picture = List.assoc_opt "picture" l ;
          description = read_description (get_field "read" "description" l)
        } in
      (info, read_t (get_field "read" "next"))
  and read_t = function
  | `String "end" -> Recipe.End
  | `List l -> Recipe.Step (List.map read l)
  (* [`Assoc l], with each key representing a parameter (quantity of ingredients chosen, ingredient chosen, etc.) *)
  | _ -> failwith "from_json: read_t" in
  match Yojson.Safe.from_string fileContent with
  | `Assoc l -> read_t (get_field "from_json" "recipes" l)
  | _ -> failwith "from_json: Not an object"


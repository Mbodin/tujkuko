
open ExtLib

let print_item = function
  | Recipe.Sentence str -> `String str
  | Recipe.Unit (min, max, u) ->
    `Assoc ([
        ("min", `Float min) ;
        ("max", `Float max) ;
      ] @
        match u with
        | None -> [("unit", `Bool false)]
        | Some u -> [
            ("unit", `Bool true) ;
            ("notation", `String u.Recipe.munit_notation) ;
            ("metric", `Bool u.Recipe.munit_metric)
          ])

let print_step s = `List (List.map print_item s)

let print_description d =
  `Assoc (PMap.foldi (fun lg s l -> (lg, print_step s) :: l) d [])

let print_hints d =
  `Assoc (PMap.foldi (fun lg h l -> (lg, `List (List.map print_step h)) :: l) d [])

let rec print_t = function
  | Recipe.End -> `String "end"
  | Recipe.Step l -> `List (List.map print l)
and print (i, t) =
  `Assoc (
    (Option.map_default (fun p -> [("picture", `String p)]) [] i.Recipe.picture)
    @ [
      ("description", print_description i.Recipe.description) ;
      ("hints", print_hints i.Recipe.hints) ;
      ("next", print_t t)
    ])

let to_json r =
  (* LATER: pretty-printed JSON can be quite large, but compact ones behaves badly with diffs.
     A compromise has to be find. *)
  Yojson.Safe.pretty_to_string ~std:true (`Assoc [("recipes", print_t r)])

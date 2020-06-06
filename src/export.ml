
let print_item = function
  | Recipe.Sentence str -> `String str
  | Recipe.Unit (min, max, u) ->
    `Assoc [
        ("min", `Int min) ;
        ("max", `Int max) ;
        ("notation", `String u.Recipe.munit_notation) ;
        ("metric", `String (if u.Recipe.munit_metric then "yes" else "no"))
      ]

let print_step s = `List (List.map print_item s)

let print_description d =
  `Assoc (PMap.fold (fun (lg, s) l -> (lg, print_step s) :: l) d [])

let rec print_t = function
  | Recipe.End -> `String "end"
  | Recipe.Step l -> `List (List.map print l)
and print (i, t) =
  `Assoc (
    (Option.to_list (Option.map (fun p -> ("picture", p)) i.Recipe.picture))
    @ [
      ("description", print_description i.description) ;
      ("next", print_t t)
    ])

let to_json r =
  Yojson.Safe.pretty_to_string ~std:true (print_t r)


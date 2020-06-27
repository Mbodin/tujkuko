
open Libutils

type path = string list

type state = {
    recipe : Recipe.t
      (** The recipe, describing what are the next steps. *) ;
    path : string BidirectionalList.t
      (** The path to get there. *)
  }

let get_path st = BidirectionalList.to_list st.path

let init t p =
  let rec aux = function
    | (t, []) -> Some t
    | (Recipe.End, _) -> None
    | (Recipe.Step l, id :: p) ->
      match List.find_opt (fun (i, _t) -> id = i.Recipe.id) l with
      | None -> None
      | Some (_i, t) -> aux (t, p) in
  match aux (t, p) with
  | Some t -> {
      recipe = t ;
      path = BidirectionalList.from_list p
    }
  | None -> {
      recipe = t ;
      path = BidirectionalList.empty
    }

let next st =
  match st.recipe with
  | Recipe.End -> None
  | Recipe.Step l ->
    Some (List.map (fun (i, t) ->
      (i, {
         recipe = t ;
         path = BidirectionalList.add_right st.path i.Recipe.id
       })) l)


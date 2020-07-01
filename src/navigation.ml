
open Libutils

type path = string list

(** A type to store the context from the root recipe. *)
type context =
  | Hole (** A simple hole: the context is empty. *)
  | ContextStep of (Recipe.info * Recipe.t) list
                   * Recipe.info
                   * (Recipe.info * Recipe.t) list
                   * context
  (** A context step, corresponding to [Recipe.Step], with the hole in the middle of the list.
     The list is divided into two lists: the list appearing before, and the list appearing after.
     Note that this is a zipper: the top context corresponds to the last applied context.
     The first sublist is not reversed (as can sometimes be seen with zippers). *)

type state = {
    recipe : Recipe.t
      (** The recipe, describing what are the next steps. *) ;
    context : context
      (** The context to get there. *)
  }

let rec context_to_path = function
  | Hole -> []
  | ContextStep (_, i, _, c) -> i.Recipe.id :: context_to_path c

let get_path st = context_to_path st.context

let init t p =
  (** The value to be returned if the path is invalid. *)
  let base = ({
      recipe = t ;
      context = Hole
    }, []) in
  let rec aux c is = function
    | (t, []) -> ({
        recipe = t ;
        context = c
      }, List.rev is)
    | (Recipe.End, _) -> base
    | (Recipe.Step l, id :: p) ->
      match Utils.list_predicate_prefix (fun (i, _t) -> id <> i.Recipe.id) l with
      | (l_before, (i, t) :: l_after) ->
        assert (id = i.Recipe.id) ;
        aux (ContextStep (l_before, i, l_after, c)) (i :: is) (t, p)
      | _ -> base in
  aux Hole [] (t, p)

let next st =
  match st.recipe with
  | Recipe.End -> None
  | Recipe.Step l ->
    let rec aux before = function
      | [] -> []
      | (i, t) :: after ->
        (i, {
            recipe = t ;
            context = ContextStep (BidirectionalList.to_list before, i, after, st.context)
          }) :: aux (BidirectionalList.add_right before (i, t)) after
    in
    Some (aux BidirectionalList.empty l)

let parent st =
  match st.context with
  | Hole -> None
  | ContextStep (before, i, after, c) ->
    Some {
        recipe = Recipe.Step (before @ (i, st.recipe) :: after) ;
        context = c
      }

let write st i =
  match st.context with
  | Hole -> st
  | ContextStep (before, _, after, c) ->
    { st with context = ContextStep (before, i, after, c) }

let add_child st ?(final = false) i =
  match st.recipe with
  | End -> None
  | Step l ->
    let t = if final then Recipe.End else Recipe.Step [] in
    let st' = { st with recipe = Step (l @ [ (i, t) ]) } in
    let nst = {
        recipe = t ;
        context = ContextStep (l, i, [], st.context)
      } in
    Some (st', nst)

let export st =
  let rec aux st =
    match parent st with
    | None -> st
    | Some st -> aux st in
  (aux st).recipe


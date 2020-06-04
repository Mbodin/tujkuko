(** The main module. *)

(** Given a recipe, extracts the information. *)
let get_info = function
  | Recipe.End i -> i
  | Recipe.Step (i, _) -> i

(** Given a language, explore a [Recipe.t]. *)
let explore lg t =
  let i = get_info t in
  (** Print picture if any. *)
  ???



open Libutils

(** There are currently very few features in recipes: they are already a tree,
   so we do not need to introduce an indirection. *)
type state = Recipe.t

let init = Utils.id

let next = function
  | Recipe.End -> None
  | Recipe.Step l -> Some l


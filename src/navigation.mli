(** Navigating through [Recipe.t]. *)

(** The current state of the navigation. *)
type state

(** Create a state with no past. *)
val init : Recipe.t -> state

(** List all the next steps that can be taken from the current state,
   with the state that would be reached if this direction is taken.
   If the current state is final, the function returns [None].
   Returning [Some []] means that the current state is not final, but
   that the next steps have not been translated or added. *)
val next : state -> (Recipe.info * state) list option


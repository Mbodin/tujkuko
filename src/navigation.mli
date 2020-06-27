(** Navigating through [Recipe.t]. *)

(** The current state of the navigation. *)
type state

(** The list of step identifiers to identify a state. *)
type path = string list

(** Returns the path to get to the current state. *)
val get_path : state -> path

(** Create a state from a path.
   If the path is invalid, the created step will be with no past. *)
val init : Recipe.t -> path -> state

(** List all the next steps that can be taken from the current state,
   with the state that would be reached if this direction is taken.
   If the current state is final, the function returns [None].
   Returning [Some []] means that the current state is not final, but
   that the next steps have not been translated or added. *)
val next : state -> (Recipe.info * state) list option

(* TODO: Timers might preemptively trigger a step to be automatically added into the history. *)


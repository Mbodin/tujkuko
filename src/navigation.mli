(** Navigating through [Recipe.t]. *)

(** The current state of the navigation. *)
type state

(** The list of step identifiers to identify a state. *)
type path = string list

(** Returns the path to get to the current state. *)
val get_path : state -> path

(** Create a state from a path.
   If the path is invalid, the created state will be a valid prefix of the provided path.
   The list of recipe information traversed to get there, as well as their associated states,
   is also returned. *)
val init : Recipe.t -> path -> state * (Recipe.info * state) list

(** List all the next steps that can be taken from the current state,
   with the state that would be reached if this direction is taken.
   If the current state is final, the function returns [None].
   Returning [Some []] means that the current state is not final, but
   that the next steps have not been translated or added. *)
val next : state -> (Recipe.info * state) list option

(** Get the parent of the current state, cancelling the last choice made.
   If there is no parent, the function returns [None]. *)
val parent : state -> state option

(** Overwrite the information associated to the current state.
   The root state has no associated information: this function behaves
   as the identity function on it. *)
val write : state -> Recipe.info -> state

(** Add a child to the current node.
   The state associated to the inserted node is returned after the update state.
   If the current recipe is [Recipe.End], no child can be added and the function returns [None].
   The argument [final] states wether the added argument is in turn a [Recipe.End] (default
   [false]). *)
val add_child : state -> ?final:bool -> Recipe.info -> (state * state) option

(** Get back the initial [Recipe.t] object (possibly updated). *)
val export : state -> Recipe.t

(* TODO: Timers might preemptively trigger a step to be automatically added into the history. *)


(** Define the data structures to store recipes. *)

(** Each language is identified by its ISO 639 code. *)
type language = string

(** A step is a list of items. *)
type item =
  | Sentence of string
  | Number of int
type step = item list

(** Information that can be stored at any point of the step. *)
type info = {
    picture : string option
    (** An optional picture of the step. *) ;
    description : (language, step) PMap.t
    (** For each language, a description of the step. *)
  }

(** The main type: a tree with all the recipes.
    The constructor [End] states that the recipe is finished.  This is different than a [Step] with
    no subtrees in the sense that no further step are meant to be added afterwards. *)
type t =
  | End of info
  | Step of info * t list


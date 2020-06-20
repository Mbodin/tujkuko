(** Define the data structures to store recipes. *)

(** Each language is identified by its ISO 639 code. *)
type language = string

(** The type of measuring units. *)
type munit = {
    munit_notation : string (** The notation of the unit. *) ;
    munit_metric : bool (** Whether it accepts metric prefixes. *)
  }

(** A step is a list of items. *)
type item =
  | Sentence of string (** A string that will be displayed as-is. *)
  | Unit of float * float * munit option
    (** An expected quantity, with a minimum and maximal values
      (which can be equal), and an optional unit. *)
  (* LATER: timers, menu lists, links to OpenFoodFacts, etc. *)

type step = item list

(** Information that can be stored at any point of the step. *)
type info = {
    picture : string option
      (** An optional picture of the step. *) ;
    description : (language, step) PMap.t
      (** For each language, a description of the step. *) ;
    hints : (language, step list) PMap.t
      (** For each language, a list of hints. *) ;
  }

(** The main type: a tree with all the recipes.
    The constructor [End] states that the recipe is finished.  This is different than [Step []]
    in the sense that no further step are meant to be added afterwards. *)
type t =
  | End (* LATER: Additional informations? Like calories, number of people, etc. *)
  | Step of (info * t) list
  (* LATER: | Parallel of t list * t (** A list of recipes to be done before what follows *) *)
(* Alternatively, do we want to switch to a graph? Or a rule-based formalism? *)


(** Importing recipes. *)

(** Import recipes from a JSON file. *)
val from_json : string -> Recipe.t

(** [import_translations fileName fileContent] reads [fileContent] as a
   JSON object representing translations in different languages.
   It then returns a translation object (associating a pair of identifier
   and language code to its translation) as well as the list of languages
   codes found in the file. *)
val import_translations : string -> string -> (string * string, string) PMap.t * string list


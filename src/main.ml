(** The main module. *)

open Libutils
open ExtList
open ExtString

(** This entire file is parameterised by an interface as specified in the
   InOut Module. *)
module Main (IO : InOut.T) = struct

let webpage_link = "https://github.com/Mbodin/tujkuko"
let webpage_issues = "https://github.com/Mbodin/tujkuko/issues"

(** URL tag *)
let urltag_lang = "lang"

(** A trace of the menu function to help debugging. *)
let trace = ref ["init"]

(** Adding a message in the trace *)
let add_trace msg = trace := msg :: !trace

(** Get the full trace. *)
let get_trace _ = List.rev !trace

(** The default error messages. *)
let errorTranslationsDefault =
  ("An error occurred!", "Please report it", "there", "Error details:")

(** The translations needed to print error messages. *)
let errorTranslations = ref errorTranslationsDefault

let file_signature file =
  string_of_int (Hashtbl.hash file)

(** Getting and parsing the translations file. *)
let get_translations _ =
  let%lwt (translation, languages) =
    let translations_file = "translations.json" in
    let%lwt translations = IO.get_file translations_file in
    add_trace ("getting translation file (" ^ file_signature translations_file ^ ")") ;
    Lwt.return (Import.import_translations translations_file translations) in
  (** Shuffling languages, but putting the user languages on top. *)
  let (matching, nonmatching) =
    List.partition (fun lg ->
      List.exists (fun ulg -> String.exists ulg lg) IO.languages) languages in
  Lwt.return (translation, List.sort matching @ List.sort nonmatching)

(** The main script. *)
let main =
  try%lwt
    IO.clear_response () ;
    let%lwt (translation, languages) = get_translations () in
    add_trace "Translations ready" ;
    let translate_find lg key =
      try Some (PMap.find (key, lg) translation)
      with Not_found -> None in
    let get_translation_language_direct lg key =
      Utils.assert_option ("No key `" ^ key ^ "' found for language `"
                           ^ lg ^ "' at " ^ __LOC__ ^ ".")
        (translate_find lg key) in
    let get_translation lg key =
      match translate_find lg key with
      | Some tr -> tr
      | None ->
        let missing = get_translation_language_direct lg "missing" in
        "<" ^ missing ^ " `" ^ key ^ "'>" in
    (** We request the recipes without forcing it yet. *)
    let recipes =
      let%lwt file = IO.get_file "data/recipes.json" in
      Lwt.return (Import.from_json file) in

    let rec ask_for_languages _ =
      (** Showing to the user all available languages. *)
      add_trace "ask_for_languages" ;
      IO.set_parameters [] ;
      errorTranslations := errorTranslationsDefault ;
      let%lwt language =
        let (cont, w) = Lwt.task () in
        IO.print_block (InOut.Div (InOut.Normal, List.map (fun lg ->
          let get_translation = get_translation lg in
          InOut.Div (InOut.Centered, [
            InOut.P [ InOut.LinkContinuation (true, InOut.Button true, get_translation "name",
              fun _ ->
                IO.clear_response () ;
                errorTranslations :=
                  (get_translation "error", get_translation "report",
                   get_translation "there", get_translation "errorDetails") ;
                add_trace ("language chosen: " ^ lg) ;
                Lwt.wakeup_later w lg) ]])) languages)) ;
        IO.stopLoading () ;%lwt
        cont in
      start language

    and start lg =
      add_trace "start" ;
      let (cont, w) = Lwt.task () in
      let get_translation = get_translation lg in
      IO.set_parameters [(urltag_lang, lg)] ;
      let%lwt recipes = recipes in
      IO.stopLoading () ;%lwt
      IO.print_block (InOut.Div (InOut.Centered, [
          InOut.LinkContinuation (false, Button false,
            get_translation "backToLanguages",
            (fun _ ->
              IO.clear_response () ;
              Lwt.wakeup_later w ask_for_languages))
        ])) ;
      IO.print_block (InOut.P [
          InOut.Text (get_translation "welcome") ;
          InOut.Text (get_translation "free") ;
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link) ;
        ]) ;
      IO.print_block (InOut.P [InOut.Text "TODO"]) ;
(* TODO
(** Given a language, explore a [Recipe.t]. *)
let explore lg t =
  let i = get_info t in
  (** Print picture if any. *)
  ???
*)
      let%lwt cont = cont in cont () in

    (** Setting the environment. *)
    let arguments = IO.get_parameters () in
    let lg =
      match List.assoc_opt urltag_lang arguments with
      | None -> None (** No language is provided. *)
      | Some lg ->
        match translate_find lg "iso639" with
        | None -> None (** Invalid language. *)
        | Some _ -> Some lg in
    match lg with
    | None -> ask_for_languages ()
    | Some lg -> start lg

  (** Reporting errors. *)
  with e ->
    try%lwt
      let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
      IO.print_block ~error:true (InOut.Div (InOut.Normal, [
          InOut.P [
              InOut.Text errorOccurred ; InOut.Text reportIt ;
              InOut.LinkExtern (InOut.Simple, there, webpage_issues)
            ] ;
          InOut.P [
              InOut.Text errorDetails ;
              InOut.Text (Printexc.to_string e) ;
              InOut.Text "; " ;
              InOut.Text (String.concat ", " (get_trace ()))
            ]
        ])) ;
      IO.stopLoading () ;%lwt
      Lwt.return ()
    with e' -> (** If there have been an error when printing the error,
                  we failback to the console. *)
      IO.log "Unfortunately, a important error happened." ;
      IO.log ("Please report it to " ^ webpage_issues) ;
      IO.log ("Primary error details: " ^ Printexc.to_string e) ;
      IO.log ("Secondary error details: " ^ Printexc.to_string e') ;
      IO.log ("Trace: " ^ String.concat "; " (get_trace ())) ;
      Lwt.return ()

end


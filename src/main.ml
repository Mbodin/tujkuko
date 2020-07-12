(** The main module. *)

open Libutils
open ExtList
open ExtString

(** This entire file is parameterised by an interface as specified in the
   InOut Module. *)
module Main (IO : InOut.T) = struct

let webpage_link = "https://github.com/Mbodin/tujkuko"
let webpage_issues = "https://github.com/Mbodin/tujkuko/issues"
let webpage_icons = "http://poufpoufproduction.fr/"

(** URL tags *)
let urltag_lang = "lang"
let urltag_path = "path"

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
    add_trace (Printf.sprintf "getting translation file (%s)" (file_signature translations_file)) ;
    Lwt.return (Import.import_translations translations_file translations) in
  (** Putting the user language on top. *)
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
      Utils.assert_option (Printf.sprintf "No key `%s' found for language `%s' at %s." key lg __LOC__)
        (translate_find lg key) in
    let get_translation lg key =
      match translate_find lg key with
      | Some tr -> tr
      | None ->
        let missing = get_translation_language_direct lg "missing" in
        Printf.sprintf "<%s `%s'>" missing key in
    (** We request the recipes without forcing it yet. *)
    let recipes =
      let%lwt file = IO.get_file "data/recipes.json" in
      Lwt.return (Import.from_json file) in

    (** Showing to the user all available languages. *)
    let rec ask_for_languages _ =
      add_trace "ask_for_languages" ;
      IO.set_parameters [] ;
      errorTranslations := errorTranslationsDefault ;
      let%lwt language =
        let (cont, w) = Lwt.task () in
        IO.print_block (InOut.Div (InOut.Normal, [], List.map (fun lg ->
          let get_translation = get_translation lg in
          InOut.Div (InOut.Centered, [], [
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
      start language ""

    (** The main loop: display all recipes. *)
    and start lg path =
      add_trace (Printf.sprintf "start (%s)" path) ;
      let (cont, w) = Lwt.task () in
      let get_translation = get_translation lg in
      let set_parameters path =
        IO.set_parameters [
            (urltag_lang, lg) ;
            (urltag_path, String.concat " " path)
          ] in
      let path = String.split_on_char ' ' path in
      set_parameters path ;
      let%lwt recipes = recipes in
      (** Generate a node given a [Recipe.step]. *)
      let step_to_node s =
        let item_to_block = function
          | Recipe.Sentence str -> InOut.Text str
          | Recipe.Unit (min, max, correlation, u) ->
            ignore (min, max, correlation, u) ; InOut.Text "" (* TODO *) in
        IO.block_node (InOut.P (List.map item_to_block s)) in
      let (initial_nav_state, initial_infos) = Navigation.init recipes path in
      (** The state of the interface, expressed as:
       - the current [Navigation.state],
       - the current list of hints,
       - (* TODO: Some notion of “factor” to multiply each units. *)
       - a list of tuple for each item in the stack:
         - the associated [Recipe.info],
         - the parent’s hints,
         - a function to remove the said element. *)
      let state =
        let hint_init = [
          step_to_node [ Recipe.Sentence (get_translation "chooseStep") ] ] in
        ref (initial_nav_state, hint_init, () (* TODO: the factor *), []) in
      (** Menu buttons. **)
      let (save_button, update_save_button) = IO.controlableNode (IO.block_node InOut.Space) in
      update_save_button (IO.block_node (InOut.LinkContinuation (true, Button false,
        get_translation "editMode",
        (fun _ ->
          (* TODO: Switching edit mode on. *)
          update_save_button (IO.block_node (InOut.LinkContinuation (true, Button true,
            get_translation "saveEdits",
            (fun _ ->
              IO.clear_response () ;
              Lwt.wakeup_later w (fun _ ->
                let (nav_state, _, _, _) = !state in
                let path = Navigation.get_path nav_state in
                let path = String.concat " " path in
                save_edits lg path (Navigation.export nav_state)))))))))) ;
      IO.print_block ~kind:InOut.RawResponse (InOut.Div (InOut.Navigation, ["center"], [
          InOut.LinkContinuation (false, Button false,
            get_translation "backToLanguages",
            (fun _ ->
              IO.clear_response () ;
              Lwt.wakeup_later w ask_for_languages)) ;
          InOut.Space ;
          InOut.Node save_button
        ])) ;
      IO.print_block (InOut.P [
          InOut.Text (get_translation "welcome") ;
          InOut.Text (get_translation "free") ;
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_link) ;
        ]) ;
      IO.print_block (InOut.P [
          InOut.Text (get_translation "iconsDescription") ;
          InOut.LinkExtern (InOut.Simple, get_translation "there", webpage_icons) ;
        ]) ;
      (** The node storing all the steps that were done until now. *)
      let (past, add_past) = IO.extendableList () in
      IO.print_block ~kind:InOut.RawResponse
        (InOut.Div (InOut.Normal, ["history"], [ InOut.Node past ])) ;
      (** The node storing the hints. *)
      let (hints, update_hints) =
        IO.controlableNode (IO.block_node InOut.Space) in
      let update_hints hint_list =
        update_hints (IO.block_node (InOut.List (false,
          List.map (fun n -> [ InOut.Div (InOut.Normal, [], [ InOut.Node n ]) ]) hint_list))) in
      IO.print_block  ~kind:InOut.RawResponse
        (InOut.Div (InOut.Normal, ["hints"], [ InOut.Node hints ])) ;
      (** The node storing the next possible steps. *)
      let (next, update_next) = IO.controlableNode (IO.block_node InOut.Space) in
      IO.print_block  ~kind:InOut.RawResponse
        (InOut.Div (InOut.Normal, ["future"], [ InOut.Node next ])) ;
      (** To be called at every interface change. *)
      let update _ =
        let (nav_state, _, _, _) = !state in
        set_parameters (Navigation.get_path nav_state) in
      (** Fetch the hints from a [Recipe.info]. *)
      let get_hints info =
        try
          let hs = PMap.find lg info.Recipe.hints in
          List.map step_to_node hs
        with Not_found -> [] in
      (** To be called when the current state has changed and the hints
         and next steps have to be updated. *)
      let rec update_local _ =
        update () ;
        let (nav_state, hints, _, _) = !state in
        update_hints hints ;
        let next =
          match Navigation.next nav_state with
          | None -> InOut.Space
          | Some l ->
            InOut.List (false,
              Utils.list_map_filter (fun (i, st) ->
                match try Some (PMap.find lg i.Recipe.description)
                      with Not_found -> None with
                | None -> None
                | Some s ->
                  let n = step_to_node s in
                  let n =
                    (* TODO: Print picture, if any. *) n in
                  let n =
                    let inter = IO.clickableNode n in
                    inter.IO.onChange (fun _ ->
                      move_down i st ;
                      update_local ()) ;
                    inter.IO.node in
                  let n =
                    match Navigation.next st with
                    | None ->
                      (** This step is a final one. *)
                      IO.addClass ["finalStep"] n
                    | Some _ -> n in
                  Some [ InOut.Node n ]) l) in
        update_next (IO.block_node next)
      (** Move one step up the tree. *)
      and move_up _ =
        let (nav_state, _, factor, stack) = !state in
        match Navigation.parent nav_state with
        | None -> ()
        | Some nav_state ->
          match stack with
          | [] -> assert false
          | (_, hints, remove) :: stack ->
            remove () ;
            state := (nav_state, hints, factor, stack)
      (** Repeatively calls [move_up] until the right stack legnth is reached. *)
      and fit_to_stack_length l =
        let (_, _, _, stack) = !state in
        let current = List.length stack in
        assert (l <= current) ;
        let rec aux = function
          | 0 -> ()
          | n -> move_up () ; aux (n - 1) in
        aux (current - l)
      (** Move one step down down the tree, given the associated [Recipe.info]
         and [Navigation.state]. *)
      and move_down info nav_state =
        let step =
          try PMap.find lg info.Recipe.description
          with Not_found -> assert false in
        let p = step_to_node step in
        let p = IO.clickableNode p in
        let remove_p = add_past p.IO.node in
        let (_, hints, factor, stack) = !state in
        let current_hints = get_hints info in
        let current_hints =
          match Navigation.next nav_state with
          | None -> (** We reached a final state. *)
            current_hints @ [ step_to_node [ Recipe.Sentence (get_translation "bonappetit") ] ]
          | Some _ -> current_hints in
        state := (nav_state, current_hints, factor, (info, hints, remove_p) :: stack) ;
        p.IO.onChange (fun _ ->
          (** The user wants to go back. *)
            fit_to_stack_length (List.length stack) ;
            update_local ()) in
      List.iter (fun (i, st) -> move_down i st) initial_infos ;
      update_local () ;
      IO.stopLoading () ;%lwt
      let%lwt cont = cont in cont ()

    (** Save the edits of the recipe. *)
    and save_edits lg path t =
      add_trace (Printf.sprintf "save_edits (%s)" path) ;
      let (cont, w) = Lwt.task () in
      let get_translation = get_translation lg in
      IO.print_block ~kind:InOut.RawResponse (InOut.Div (InOut.Navigation, ["center"], [
          InOut.LinkContinuation (false, Button false,
            get_translation "backToRecipe",
            (fun _ ->
              IO.clear_response () ;
              Lwt.wakeup_later w (fun _ -> start lg path)))
        ])) ;
      IO.print_block (InOut.Div (InOut.Normal, [], [
          InOut.P [ InOut.Text (get_translation "listOfSaveMethods") ] ;
          InOut.List (true, [
              [ InOut.LinkFile (InOut.Button true, get_translation "methodJSONButton",
                  "recipes.json", "application/json", true, (fun _ -> Export.to_json t)) ;
                InOut.Text (get_translation "methodJSON") ]
            ])
        ])) ;
      IO.print_block ~kind:InOut.ErrorResponse (InOut.Text ("TODO")) (* TODO *) ;
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
    | Some lg ->
      let path =
        match List.assoc_opt urltag_path arguments with
        | None -> ""
        | Some p -> p in
      start lg path

  (** Reporting errors. *)
  with e ->
    try%lwt
      let (errorOccurred, reportIt, there, errorDetails) = !errorTranslations in
      IO.print_block ~kind:InOut.ErrorResponse (InOut.Div (InOut.Normal, [], [
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


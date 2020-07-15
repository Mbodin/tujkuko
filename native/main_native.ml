(** Module Main_native.
   Instantiates Module Main for the native output. *)

module Main = Tujkuko.Main.Main (Blocklib_native.InOut)

let _ = Lwt_main.run Main.main


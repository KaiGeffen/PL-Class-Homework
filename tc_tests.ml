(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Tc

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

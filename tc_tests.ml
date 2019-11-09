(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Tc
open Tc_util

(* Helper methods for testing *)
let test_tc  ?(g : typeEnv = []) (prog : string) (t : typ) : bool =
  t == tc (from_string prog) g

(* -------Constants------- *)
let%TEST "Bool has type bool" = test_tc "true" TBool && test_tc "false" TBool
let%TEST "Integer has type integer" = test_tc "3" TInt

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

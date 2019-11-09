(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Tc
open Tc_util

(* Helper methods for testing *)
let test_tc  ?(g : typeEnv = []) (prog : string) (t : typ) : bool =
  t == tc (from_string prog) g

let test_tc_throws ?(g : typeEnv = []) (prog : string) : bool =
  try let _ = tc (from_string prog) g in false
  with _ -> true

(* -------Constants------- *)
let%TEST "Bool has type bool" = test_tc "true" TBool && test_tc "false" TBool
let%TEST "Integer has type integer" = test_tc "3" TInt

(* ---------Let/ID-------- *)
let%TEST "Free variable is invalid" = test_tc_throws "x"
let%TEST "Free variable is invalid even if other variables are bound" =
  test_tc_throws "let y = 3 in x"
let%TEST "Bound variable is given type" =
  test_tc "let x = false in x" TBool && test_tc "let x = 2 in x" TInt
let%TEST "Twice bound variable is the innermost type" =
  test_tc "let x = false in let x = 5 in x" TInt
let%TEST "Var bound to defined var has that vars type" =
  test_tc "let x = 3 in let y = x in y" TInt
let%TEST "Var bound to undefined var is invalid" =
  test_tc_throws "let x = y in let y = 3 in x"

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

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

(* ----------Op2--------- *)
(* TODO(kgeffen) Test LT once the parser bug is fixed
  https://github.com/plasma-umass/compsci631/issues/19  
*)
let%TEST "Algebraic operations with 2 numbers yield Int" =
  test_tc "1 + 2" TInt && test_tc "2 - 4" TInt && 
  test_tc "1 * 2" TInt && test_tc "2 / 4" TInt && 
  test_tc "1 % 2" TInt
let%TEST "Less/greater than with 2 numbers yield bool" =
  (* test_tc "0 < 0" TBool  &&  *)test_tc "3 > 0" TBool
let%TEST "Algebraic operations with a non-numbers is invalid" =
  test_tc_throws "1 + true" && test_tc_throws "false % 4"
let%TEST "Less/greater than with non-numbers is invalid" =
  (* test_tc_throws "true < false" &&  *)test_tc_throws "3 > true"
let%TEST "Equality testing ints/bools yields bool" =
  test_tc "3 == 4" TBool && test_tc "false == 5" TBool && test_tc "false == false" TBool

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Tc
open Tc_util

(* Helper methods for testing *)
let test_tc  ?(g : typeEnv = []) (prog : string) (t : typ) : bool =
  t = tc (from_string prog) g

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

(* ---------If------------ *)
let%TEST "If can return 2 ints or 2 bools" =
  test_tc "if true then 1 else 2" TInt && test_tc "if true then false else false" TBool
let%TEST "If is invalid when its return types are not the same" =
  test_tc_throws "if true then 1 else false"
let%TEST "If is invalid when its return types variables of different types" =
  test_tc_throws "let x = 1 in let y = true in if true then x else y"
let%TEST "If results can contain operations" =
  test_tc "if true then (1 + 2) else (3 * 4)" TInt
let%TEST "If condition can contain operations" =
  test_tc "if (3 == 3) then 1 else 2" TInt
let%TEST "If is invalid when it has an undefined variable" =
  test_tc_throws "if true then 3 else x"
let%TEST "If condition scope does not carry to the expressions" =
  test_tc_throws "if (let x = 4 in true) then 3 else x"
let%TEST "If results can contain operations" =
  test_tc "if true then (1 + 2) else (3 * 4)" TInt

(* ---------Fun----------- *)
let%TEST "Function from bool to int works"
  = test_tc "fun (x : bool) -> 3" (TFun (TBool, TInt))
let%TEST "Function containing its argument correctly types that argument"
  = test_tc "fun (x : bool) -> x" (TFun(TBool, TBool))
let%TEST "Function with invalid contents is invalid"
  = test_tc_throws "fun (x : bool) -> y"
let%TEST "Function argument can be a function"
  = test_tc "fun (x : (int -> int)) -> true" (TFun(TFun(TInt, TInt), TBool))
let%TEST "Function contents can be a non-trivial expression"
  = test_tc "fun (x : bool) -> if x then 3 else 5" (TFun(TBool, TInt))
let%TEST "Function arg type overrules that variables type inside the function" =
  test_tc "let x = 3 in fun (x : bool) -> true" (TFun(TBool, TBool))
let%TEST "Function arg type is not in scope outside of the function" =
  test_tc "let x = 3 in let _ = fun (x : bool) -> true in x" TInt






(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

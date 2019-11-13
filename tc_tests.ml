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

(* ----------Fun---------- *)
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
let%TEST "Function of multiple arguments has correct type" =
  test_tc "fun (x : int) -> (fun (y : int) -> (x + y))" (TFun(TInt, TFun(TInt, TInt)))

(* ---------App------------ *)
let%TEST "App of basic function works when given right arg type" =
  test_tc "(fun (x : bool) -> 3) true" TInt
let%TEST "App of basic function is invalid when given incorrect arg type" =
  test_tc_throws "(fun (x : bool) -> 3) 7"
let%TEST "App works for functions expecting function type arg" =
  test_tc "(fun (f : int -> int) -> f 7) (fun (x : int) -> 1 + x)" TInt
let%TEST "App for function expecting function type arg throws when given wrong arg type" =
  test_tc_throws "(fun (f : int -> int) -> f 7) 8"
let%TEST "App for function of multiple args returns curried function type if only 1 arg applied" =
  test_tc "(fun (x :int) -> (fun (y : int) -> x + y)) 4" (TFun(TInt, TInt))
let%TEST "App for function of multiple args returns the final type when all args applied" =
  test_tc "(fun (x :int) -> (fun (y : int) -> x + y)) 4 5" TInt
let%TEST "App can have a bound variable as the applied argument" =
  test_tc "let x = 3 in (fun (x : int) -> 1 + x) x" TInt

(* ----------Fix----------- *)
let%TEST "Fix with basic int works" =
  test_tc "fix (x : int) -> 3" TInt
let%TEST "Fix fails when the signature is a bool but the return is int" =
  test_tc_throws "fix (x : bool) -> 5"
let%TEST "Fix implementing factorial is int" =
  test_tc "let y = 5 in fix (x : int) -> (if y == 0 then 1 else (y * (let y = y-1 in x)))" TInt
let%TEST "Fix arg overrules that variables type inside the fix" =
  test_tc "let x = true in fix (x : int) -> x" TInt
let%TEST "Fix arg is not in scope outside the function" =
  test_tc_throws "if (fix (x : bool) -> true) then x else x"
let%TEST "Fix arg can be a function" =
  test_tc "fix (x : int -> bool) -> fun (x : int) -> if x > 0 then true else false" (TFun(TInt, TBool))
let%TEST "Fix can be non-terminating and still be correctly type-checked" =
  test_tc "fix (x : int) -> x" TInt

(* --------Empty/is_empty------- *)
let%TEST "Empty int list is an int list" =
  test_tc "empty<int>" (TList TInt)
let%TEST "Empty list can be a type containing lists and functions" =
  test_tc "empty<(int list) -> bool>" (TList (TFun((TList TInt), TBool)))
let%TEST "is_empty is bool when given a list" =
  test_tc "is_empty empty<int>" TBool
let%TEST "is_empty is bool when given any expression" =
  test_tc "is_empty (if true then 3 else 4)" TBool
let%TEST "is_empty is bool even when the expression is invalid" =
  test_tc "is_empty x" TBool
let%TEST "is_empty is bool when given a non-empty list" =
  test_tc "is_empty (1 :: 3)" TBool

(* -----------List------------ *)
let%TEST "List with single int is an int list" =
  test_tc "1 :: empty<int>" (TList TInt)
let%TEST "List with single int followed by empty boolean is invalid" =
  test_tc_throws "1 :: empty<bool>"
let%TEST "List of mixed basic types is invalid" =
  test_tc_throws "1 :: true"
let%TEST "List with multiple ints is int list" =
  test_tc "1 :: 2 :: 3 :: 4" (TList TInt)
let%TEST "List of bound variables is typed same as those variables" =
  test_tc "let x = 3 in let y = 1 in x :: y" (TList TInt)
let%TEST "List of same-type functions works" =
  test_tc "let f = fun (x : int) -> x in let g = fun (x : int) -> 2 + x in f :: g" (TList (TFun(TInt, TInt)))
let%TEST "List of differently typed functions is invalid" =
  test_tc_throws "let f = fun (x : bool) -> x in let g = fun (x : int) -> 2 + x in f :: f :: g"
let%TEST "List of same-type lists works" =
  test_tc "let l1 = 1 :: 3 in let l2 = 4 :: 4 in l1 :: l2" (TList (TList TInt))
let%TEST "List of differently typed lists is invalid" =
  test_tc_throws "let l1 = true :: true in let l2 = 4 :: 4 in l1 :: l2"

(* ---------Head/tail---------- *)
let%TEST "Head of single int list is int" =
  test_tc "head 3 :: empty<int>" TInt
let%TEST "Tail of single int list is int list (Even though the program is invalid)" =
  test_tc "tail 3 :: empty<int>" (TList TInt)
let%TEST "Head of function list is that function's type" =
  test_tc "head (fun (x : int) -> true) :: empty<int -> bool>" (TFun (TInt, TBool))
let%TEST "Tail of function list is that list of that function's type" =
  test_tc "tail (fun (x : int) -> true) :: (fun (y : int) -> y == 4)" (TList (TFun (TInt, TBool)))

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

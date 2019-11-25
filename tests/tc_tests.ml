(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Tc
open Tc_util

(* Helper methods for testing *)
let test_tc (prog : string) (t : typ) : bool =
  t = get_type (from_string prog)

let test_tc_throws (prog : string) : bool =
  try let _ = get_type (from_string prog) in false
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
let%TEST "List expressions which define variables are not in scope outside of that element" =
  test_tc_throws "(let x = 3 in x) :: x" && test_tc_throws "x :: (let x = 3 in x)"
let%TEST "List of an int followed by an int list is valid" =
  test_tc "let l = 2 :: 3 in 1 :: l" (TList TInt)
let%TEST "List of int plus int list list is invalid" =
  test_tc_throws "1 :: ((2::2) :: (3::3))"

(* ---------Head/tail---------- *)
let%TEST "Head of single int list is int" =
  test_tc "head (3 :: empty<int>)" TInt
let%TEST "Tail of single int list is int list (Even though the program is invalid)" =
  test_tc "tail (3 :: empty<int>)" (TList TInt)
let%TEST "Head of function list is that function's type" =
  test_tc "head ((fun (x : int) -> true) :: empty<int -> bool>)" (TFun (TInt, TBool))
let%TEST "Tail of function list is that list of that function's type" =
  test_tc "tail ((fun (x : int) -> true) :: (fun (y : int) -> y == 4))" (TList (TFun (TInt, TBool)))

(* ----------Array------------- *)
let%TEST "Array full of true is bool array" =
  test_tc "array (3, true)" (TArr TBool)
let%TEST "Array with non-int length is invalid" =
  test_tc_throws "let x = (3 :: 4) in array (x, true)"
let%TEST "Array declaration can contain bound variables" =
  test_tc "let x = (3 :: 4) in array (3, x)" (TArr (TList TInt))
let%TEST "Array of functions works" =
  test_tc "let f = (fun (x : int) -> 3 * x) in array (3, f)" (TArr (TFun (TInt, TInt)))

let%TEST "GetArray returns type within the given array for simple array" =
  test_tc "array (3, true)[0]" TBool
let%TEST "GetArray invalid when a non-int is given for the index" =
  test_tc_throws "array (3, true)[true]"
let%TEST "GetArray returns type within the given array for an array of lists" =
  test_tc "let l = (2::3::4) in array (3, l)[0]" (TList TInt)

let%TEST "SetArray returns int when setting to an int array" =
  test_tc "array (3, 3)[1] = 0" TInt
let%TEST "SetArray is invalid when the index is not an int" =
  test_tc_throws "array (3, 3)[true] = 0"
let%TEST "SetArray is invalid when the value it attempts to set is of a different type than the array" =
  test_tc_throws "array (3, 3)[1] = true"
let%TEST "SetArray works even when the index is outside of range" =
  test_tc "array (3, 3)[100] = 0" TInt

(* -----------Record------------- *)
let%TEST "Empty record has record type" = test_tc "{}" (TRecord[])
let%TEST "Single element record is typed correctly" = test_tc "{x: 3}" (TRecord[("x", TInt)])
let%TEST "Multi-element record is typed correctly" =
  test_tc "{x: 3, y: true, z: empty<int>}" (TRecord[("x", TInt);("y", TBool);("z", TList TInt)])
(* TODO Decide what happens in case of duplicate fields. *)
(* Doesn't seem responsibility of tc to fail, maybe interp? *)
(* let%TEST "Records can't have duplicate fields" = test_tc_throws "{x: 3, x: 4}" *)

let%TEST "Record lookup works for single element record" =
  test_tc "{x: true}.x" TBool
let%TEST "Record lookup invalid when the named field is missing" =
  test_tc_throws "{x: 3}.y"
let%TEST "Record lookup works in multi-element record" =
  test_tc "{x: true, y: false, z: (2 + 2)}.z" TInt
let%TEST "Record lookup field name can be a bound variable, and doesn't affect that variable" =
  test_tc "let x = 3 in {x: true}.x" TBool && test_tc "let x = 3 in let _ = {x: true}.x in x" TInt

(* ----Type Function/Application---- *)
let%TEST "Type function of basic expression works" =
  test_tc "tfun a . 3" (TForall ("a", TInt))
let%TEST "Type function of an empty alpha list works" =
  test_tc "tfun a . empty<a>" (TForall ("a", (TList (TId("a")))))
let%TEST "Type function of a function from alpha to int works" =
  test_tc "tfun a . (fun (x : a) -> 3)" (TForall ("a", (TFun (TId("a"), TInt))))
let%TEST "Type function id can be have the same name as an in-scope variable and neither overrides the other" =
  test_tc "tfun a . (let a = true in fun (x : a) -> a)" (TForall ("a", (TFun (TId("a"), TBool))))
let%TEST "Type function can have multiple types as arguments" =
  test_tc "tfun a . (tfun b . (empty<a -> b>))" (TForall ("a", TForall ("b", TList (TFun (TId("a"), TId("b"))))))
let%TEST "A function can have 2 generic arguments with the same id" =
  test_tc "tfun a . (fun (x : a) -> fun (y : a) -> x == y)" (TForall ("a", (TFun (TId("a"), TFun (TId("a"), TBool)))))
(* Not sure about this one *)
let%TEST "If 2 separate type ids must be the same to avoid a type error, the program is invalid" =
  test_tc_throws "tfun b . tfun a . (empty<a> :: empty<b>)"
  (* test_tc "tfun b . (tfun a . (fun (x : a) -> fun (y : b) -> x == y))" (TForall ("a", (TFun (TId("a"), TFun (TId("a"), TBool))))) *)

let%TEST "Type application for basic expression works" =
  test_tc "(tfun a . 3 )<bool>" TInt
let%TEST "Type application for alpha list becomes the applied type list" =
  test_tc "(tfun a . empty<a>)<bool>" (TList TBool)
let%TEST "Type application on a function containing alpha works" =
  test_tc "(tfun a . (fun (x : a) -> 3))<bool>" (TFun (TBool, TInt))
let%TEST "Type application when a variable shares alpha's name works" =
  test_tc "(tfun a . (let a = 3 in a))<bool>" TInt
let%TEST "Type application to something besides a type function is invalid" =
  test_tc_throws "(3)<bool>"
let%TEST "Type application containing an unbound id is invalid" =
  test_tc_throws "(tfun a . 3 )<a>" && test_tc_throws "(tfun a . 3 )<b>"
let%TEST "Type application containing an in-scope type-id is valid" =
  test_tc "(tfun a . (tfun b . empty<a>))<int>" (TForall ("b", (TList TInt)))

let%TEST "Generic function applied without type application is invalid" =
  test_tc_throws "let genEq = tfun a . (fun (x : a) -> fun (y : a) -> a == b) in genEq 3"

(* TODO Exhaustive tests for type substitution *)

(* TODO Exhaustive tests for delta ok *)


(* TODO test for using the same id twice in succession *)
(* TODO Full length function *)
(* let%TEST "Generic function length works" =
  test_tc "let length "
 *)

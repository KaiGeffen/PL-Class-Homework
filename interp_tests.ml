(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Interp
open Tc_util

(* Helper methods for testing *)
let test_interp_throws ?(r : env = []) (prog : string) : bool =
  try let _ = interp (from_string prog) r in false
  with _ -> true

let test_interp  ?(r : env = []) (prog : string) (res : string) : bool =
  interp (from_string prog) r = interp (from_string res) r

(* -------ID/Let------- *)
let%TEST "Free identifier is invalid" = test_interp_throws "x" ~r:["y", Value(Const(Int 3))]
let%TEST "Bound id is that id's value" = test_interp "x" "3" ~r:["x", Value(Const(Int 3))]
let%TEST "Let binding of single variable with single occurence works" =
  test_interp "let x = 3 + 5 in x" "8"
let%TEST "Let binding of single variable with multiple occurence works" =
  test_interp "let x = 2 in x * x + x" "6"
let%TEST "Let binding x and y to add together works" =
  test_interp "let x = 3 in let y = 4 in x + y" "7"
let%TEST "Let binding x twice respects the innermost occurence" =
  test_interp "let x = 1 in let x = 2 in x" "2"
let%TEST "Let binding x can reference itself if it's been defined in scope" =
  test_interp "let x = 3 in let x = 2 * x in x" "6"
let%TEST "Let x = x in x is invalid when x isn't yet bound" =
  test_interp_throws "let x = x in x"
let%TEST "Let binding a variable doesn't persist it outside of its scope" =
  test_interp_throws "let x = (let y = 3 in 2 + y) in x + y"


(* -------Const------ *)
let%TEST "Constant number is that number" = test_interp "0" "0"
let%TEST "Constant true is true" = test_interp "true" "true"
let%TEST "Constant false is false" = test_interp "false" "false"

(* --------Op2------- *)
let%TEST "2 number addition works" = test_interp "3 + 4" "7"
(* NOTE (kgeffen) leaving it like this to test negatives, ideally find a better way *)
let%TEST "2 number subtraction works" = interp (Op2 (Sub, Const (Int 3), Const (Int 4))) [] = Const (Int (-1))
let%TEST "2 number multiplication works" = test_interp "3 * 4" "12"
let%TEST "2 number division works" = test_interp "12 / 4" "3"
let%TEST "2 number modulo works" = test_interp "12 % 5" "2"
(* TODO Test for LT when parser bug fixed: https://github.com/plasma-umass/compsci631/issues/19 *)
(* let%TEST "2 number less than works when true" = test_interp "(0 - 10) < 0" "true"
let%TEST "2 number less than works when false" = test_interp "10 < 0" "false" *)
let%TEST "2 number greater than works when true" = test_interp "5 > 3" "true"
let%TEST "2 number greater than works when false" = test_interp "(0-12) > (0-2)" "false"
let%TEST "2 number equal works when true" = test_interp "7 == 7" "true"
let%TEST "2 number equal works when false" = test_interp "(0-7) == 7" "false"
let%TEST "Equality for number and bool returns false" = test_interp "1 == true" "false"
let%TEST "Equality for bools works" = test_interp "false == false" "true"

let%TEST "Adding 3 numbers works" = test_interp "3 + 7 + 4" "14"
let%TEST "Order of operations respects parenthesis" = test_interp "(7 + 2) * 0" "0"

let%TEST "Adding bools is invalid" = test_interp_throws "true + false"
let%TEST "Dividing by 0 is invalid" = test_interp_throws "12 / 0"
let%TEST "Modding by 0 is invalid" = test_interp_throws "12 mod 0"

(* ---------If----------- *)
let%TEST "If invalid for non-bool conditional" =
  test_interp_throws "if 1 then 1 else 2"
let%TEST "If evaluates to first expression when true" = test_interp "if true then 1 else 2" "1"
let%TEST "If evaluates to second expression when true" = test_interp "if false then 1 else 2" "2"
let%TEST "If can have an more than a const in its conditional" = test_interp "if (7 > 4) then 1 else 2" "1"
let%TEST "If allows errors in the expression it doesn't run" =
  not (test_interp_throws "if false then err else 2")
let%TEST "If - Variables in scope don't affect outside the if statement" =
  test_interp "let x = 3 in let y = (if x == 3 then (let x = 0 in 1) else err) in x * y" "3"

(* -----Fun/Fix/App------- *)
let%TEST "Functions are valid return values" = not (test_interp_throws "fun (x : int) -> x")
let%TEST "Applying identity function works" = test_interp "(fun (x : int) -> x) 3" "3"
let%TEST "Functions can be passed as variables and applied" =
  test_interp "let foo = fun (x : int) -> 2*x in foo 3" "6"
let%TEST "Functions retain the env they were defined in" =
  test_interp "let a = 1 in let foo = fun (x : int) -> a*x in let a = 2 in foo 1" "1"
let%TEST "Functions can take functions as arguments" =
  test_interp "let double = fun (x : int) -> 2*x in let lucky = fun (f : int -> int) -> f 7 in lucky double" "14"
let%TEST "Fix works when no recursion occurs" = test_interp "fix (x : bool) -> if false then x else 3" "3"
let%TEST "Fix recursive implementation of factorial works" =
  test_interp "let y = 5 in fix (x : int) -> (if y == 0 then 1 else (y * (let y = y-1 in x)))" "120"

(* --------Lists--------- *)
let%TEST "Empty list is a value" = not (test_interp_throws "empty<int>")
let%TEST "Single int list is a value" = not (test_interp_throws "1::empty<int>")
let%TEST "Single bool list is a value" = not (test_interp_throws "false::empty<bool>")
let%TEST "Single closure list is a value" = not (test_interp_throws "(fun (x :int) -> 3*x)::empty<int -> int>")
let%TEST "Single list list is a value" = not (test_interp_throws "(1::empty<int>)::empty<int list>")
(* NOTE(kgeffen) Mixed type lists are currently valid, but aren't intentional, so aren't tested for *)
let%TEST "List containing operations performs those operations" =
  test_interp "(1+2)::(3*4)::empty<int>" "3::12::empty<int>"
let%TEST "Lists can contain in scope variables" =
  test_interp "let x = 5 in (x+1)::(x*2)::empty<int>" "6::10::empty<int>"
let%TEST "Lists containing in-scope variables resolve those variables on creation" =
  test_interp "let lst = (let x = 5 in (x+1)::(x*2)::empty<int>) in let x = 0 in lst" "6::10::empty<int>"

let%TEST "Head of single int list works" = test_interp "head (1::empty<int>)" "1"
let%TEST "Head of single list list works" = test_interp "head (1::empty<int>)::empty<int list>" "1::empty<int>"
let%TEST "Head of multi-element list works" = test_interp "head (1::2::3::4::empty<int>)" "1"

let%TEST "Tail of empty list throws" = test_interp_throws "tail empty<int>"
let%TEST "Tail of single element list is empty" = test_interp "tail (false::empty<bool>)" "empty<bool>"
let%TEST "Tail of multi-element list is everything after head list" = test_interp "tail (1::2::3::empty<int>)" "2::3::empty<int>"

let%TEST "is_empty true for Empty" = test_interp "is_empty empty<int>" "true"
let%TEST "is_empty false for single element list" = test_interp "is_empty (1::empty<int>)" "false"
let%TEST "is_empty false for single element list where the element is an empty list" =
  test_interp "is_empty ((empty<int>)::empty<int list>)" "false"
let%TEST "is_empty false for int, bool, & closure" =
  test_interp "is_empty 1" "false" && test_interp "is_empty true" "false" && test_interp "is_empty (fun (x : int) -> 3*x)" "false"

(* ------Records------- *)
let%TEST "Records can contain all simple int, bool, closure, lists, empty lists" =
  not (test_interp_throws "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}")
let%TEST "All simple values can be gotten as fields from records" =
  test_interp "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}.x1" "1" &&
  test_interp "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}.x2" "true" &&
  test_interp "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}.x3" "(fun (x : int) -> 3*x)" &&
  test_interp "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}.x4" "1::2::empty<int>" &&
  test_interp "{x1 : 1, x2 : true, x3 : (fun (x : int) -> 3*x), x4 : 1::2::empty<int>, x5 : empty<int>}.x5" "empty<int>"
let%TEST "Records containing unbound variables throw" =
  test_interp_throws "let rec = {x1 : y} in let y = 3 in rec.x1"
let%TEST "Records with fields named the same as in-scope variables can still be gotten" =
  test_interp "let rec = {x : 4} in let x = 3 in rec.x" "4"

(* --------Arrays-------- *)
(* 
let%TEST "Array with negative length is invalid" =
  test_interp_throws "array (0 - 1, 5)"
let%TEST "Array of length zero is valid" =
  not (test_interp_throws "array (0, 5)")
let%TEST "Array length must be an int" =
  test_interp_throws "array (true, 5)"
let%TEST "Array of length 1 has given value at first index" =
  test_interp "array (1, 5)[0]" "5"
let%TEST "Multi-element array has initial value at last and middle indices" =
  test_interp "array (5, 7)[2]" "7" && test_interp "array (5, 7)[4]" "7"
let%TEST "Getting an element outside the bounds of the array is invalid" =
  test_interp_throws "array (5, true)[5]"
let%TEST "Getting an index which isn't an int is invalid" =
  test_interp_throws "array (5, true)[false]"

let%TEST "Setting the first index of an array changes that index" =
  test_interp "(array (1, true)[0] = false)[0]" "false"
let%TEST "Setting an index outside the bounds of the array is invalid" =
  test_interp_throws "array (1, true)[1] = false"
let%TEST "Setting an index of an array doesn't affect that array's other elements" =
  test_interp "(array (5, 17)[3] = 5)[1]" "17"
let%TEST "Setting an element in an array won't affect other instances of that array" =
  test_interp "let a1 = array(10, 1) in let a2 = (a1[3] = 2) in a1[3]" "1"
let%TEST "Arrays can contain in-scope variables" =
  test_interp "let x = 5 in array (10, x)[3]" "5"

let%TEST "Arrays elements can be ints, bools, functions, records, lists, arrays" =
  test_interp "array (10, 1)[0]" "1" &&
  test_interp "array (10, true)[0]" "true" &&
  test_interp "array (10, (fun (x : int) -> 2*x))[0]" "fun (x : int) -> 2*x" &&
  test_interp "array (10, {x1 : 1, x2 : 2})[0]" "{x1 : 1, x2 : 2}" &&
  test_interp "array (10, 1::2::3::empty<int>)[0]" "1::2::3::empty<int>" &&
  test_interp "array (10, array(5, 0))[0]" "array(5, 0)"
 *)

(* --Type Functions/Application---- *)
let%TEST "Type functions interpret to their exp's value" =
  test_interp "tfun x . 3" "3"
let%TEST "Type application inteprets to its exp's value" =
  test_interp "(tfun x . true)<int>" "true"
let%TEST "Type function variable names don't override existing variables" =
  test_interp "let x = 3 in tfun x . x" "3"

(* Runs all tests declared with let%TEST. This must be the last line in the file. *)
let _ = Ppx_test.Test.collect ()

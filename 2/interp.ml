

(* The second homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/interp.pdf *)

(* Notes 
	Command slash comments a line
	To build and run:
	ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test interp.d.byte; ./interp.d.byte
	Ocaml runs everything from top down, main method not necessary
	alt g is goto line
	So far this language is immutable, eg it uses rho but not sigma from the lecture 4
*)

(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

open Interp_util

(* Env is an environment in which an expression exists
	Which maps any number of named ids to values
	id and const both come from interp_utils
 *)
type env = (id * const) list

(* TODO Long match on full language, deciding not to use optional args *)
let interp (e : exp) (r : env) : exp =
	match e with
		| Id x -> failwith "not yet implemented"
		| Const c -> failwith "not yet implemented"
		| Op2 (op, e1, e2) -> failwith "not yet implemented"
		| If (e1, e2, e3) -> failwith "not yet implemented"
		| Let (x, e1, e2) -> failwith "not yet implemented"
		| Fun (x, e1) -> failwith "not yet implemented"
		| Fix (x, e1) -> failwith "not yet implemented"
		| App (e1, e2) -> failwith "not yet implemented"
		| Empty -> failwith "not yet implemented"
		| Cons (e1, e2) -> failwith "not yet implemented"
		| Head e -> failwith "not yet implemented"
		| Tail e -> failwith "not yet implemented"
		| IsEmpty e -> failwith "not yet implemented"
		| Record r -> failwith "not yet implemented"
		| GetField (e, str) -> failwith "not yet implemented"

(* Read the input in the format ./interp.d.byte program
	Where program is a filepath to a program in our grammar
	Assume a single line program (No \n, more than 0 chars)
*)
(* let _ = print_endline (show_exp (from_file Sys.argv.(1)))
let _ = print_endline (show_exp (interp (from_file Sys.argv.(1)))) *)

(* TODO lots of tests *)
let test_interp_throws (e : exp) (r : env) : bool =
	try let _ = interp e r in false
	with _ -> true;;

(* -------ID--------- *)
let%TEST "Free identifier is invalid" = test_interp_throws (Id "x") ["y", Int 3]
let%TEST "Bound id is that id's value" = interp (Id "x") ["x", Int 3] = Const (Int 3)

(* -------Const------ *)
let%TEST "Constant number is that number" = interp (Const (Int 0)) [] = Const (Int 0)
let%TEST "Constant true is true" = interp (Const (Bool true)) [] = Const (Bool true)
let%TEST "Constant false is false" = interp (Const (Bool false)) [] = Const (Bool false)

(* --------Op2------- *)
let%TEST "2 number addition works" = interp (Op2 (Add, Const (Int 3), Const (Int 4))) [] = Const (Int 7)
let%TEST "2 number subtraction works" = interp (Op2 (Sub, Const (Int 3), Const (Int 4))) [] = Const (Int (-1))
let%TEST "2 number multiplication works" = interp (Op2 (Mul, Const (Int 3), Const (Int 4))) [] = Const (Int 12)
let%TEST "2 number division works" = interp (Op2 (Div, Const (Int 12), Const (Int 4))) [] = Const (Int 3)
let%TEST "2 number modulo works" = interp (Op2 (Mod, Const (Int 12), Const (Int 5))) [] = Const (Int 2)
let%TEST "2 number less than works when true" = interp (Op2 (LT, Const (Int (-10)), Const (Int 0))) [] = Const (Bool true)
let%TEST "2 number less than works when false" = interp (Op2 (LT, Const (Int 10), Const (Int 0))) [] = Const (Bool false)
let%TEST "2 number greater than works when true" = interp (Op2 (GT, Const (Int 5), Const (Int 3))) [] = Const (Bool true)
let%TEST "2 number greater than works when false" = interp (Op2 (GT, Const (Int (-12)), Const (Int (-2)))) [] = Const (Bool false)
let%TEST "2 number equal works when true" = interp (Op2 (Eq, Const (Int (-7)), Const (Int (-7)))) [] = Const (Bool true)
let%TEST "2 number equal works when false" = interp (Op2 (Eq, Const (Int (-7)), Const (Int 7))) [] = Const (Bool false)
let%TEST "Equality for number and bool returns false" = interp (Op2 (Eq, Const (Int 1), Const (Bool true))) [] = Const (Bool false)
let%TEST "Equality for bools works" = interp (Op2 (Eq, Const (Bool false), Const (Bool false))) [] = Const (Bool true)

(* 3 + 7 + 4 = 14 *)
let%TEST "Adding 3 numbers works" =
	interp (Op2 (Add, Op2 (Add, Const (Int 3), Const (Int 7)), Const (Int 4))) [] = Const (Int 14)
(* (7 + 2) x 0 = 0 not 7 *)
let%TEST "Order of operations by the structure of expression, not pemdas" =
	interp (Op2 (Mul, Op2 (Add, Const (Int 7), Const (Int 2)), Const (Int 0))) [] = Const (Int 0)

let%TEST "Adding bools is invalid" = test_interp_throws (Op2 (Add, Const (Bool false), Const (Bool true))) []
let%TEST "Dividing by 0 is invalid" = test_interp_throws (Op2 (Div, Const (Int 12), Const (Int 0))) []
let%TEST "Modding by 0 is invalid" = test_interp_throws (Op2 (Mod, Const (Int 12), Const (Int 0))) []


(* ---------If----------- *)
let%TEST "If invalid for non-bool conditional" =
	test_interp_throws (If (Const (Int 0), Const (Int 1), Const (Int 2))) []
let%TEST "If evaluates to first expression when true" =
	interp (If (Const (Bool true), Const (Int 1), Const (Int 2))) [] = Const (Int 1)
let%TEST "If evaluates to second expression when true" =
	interp (If (Const (Bool false), Const (Int 1), Const (Int 2))) [] = Const (Int 2)
let%TEST "If can have an more than a const in its conditional" =
	interp (If (Op2 (GT, Const (Int (7)), Const (Int (4))),
		Const (Int 1), Const (Int 2))) [] = Const (Int 2)

(* -----Fun/Fix/App------- *)
(* TODO not sure about this, returning a closure might be valid and convenient for our language *)
(* let%TEST "Function definition alone is not a program" = test_interp_throws (Fun ("x", (Id "x"))) []
let%TEST "Fix definition alone is not a program" = test_interp_throws (Fix ("x", (Id "x"))) []
 *)

(* --------Empty--------- *)
(* let%TEST "Empty is an invalid return value" = test_interp_throws Empty [] idk if this is true TODO *)



(* Runs all tests declared with let%TEST. This must be the last line
   in the file. *)
let _ = Ppx_test.Test.collect ()



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

let%TEST "Unbound id is invalid" = test_interp_throws (Id "x") ["y", Int 3]
let%TEST "Bound id is that id's value" = interp (Id "x") ["x", Int 3] = Const (Int 3)



(* Runs all tests declared with let%TEST. This must be the last line
   in the file. *)
let _ = Ppx_test.Test.collect ()

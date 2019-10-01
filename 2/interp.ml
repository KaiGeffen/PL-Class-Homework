

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

(* Lookup the given identifier in the given environment
	return it as a value if it's defined
 *)
let rec lookup (x : id) (r : env) : const option =
	match r with
		| [] -> None
		| (hd_id, hd_v) :: tl -> if x = hd_id then Some (hd_v) else lookup x tl
	
(* Perform the given binary operation with the expression
	For invalid applications (eg true + 3), fail
	Otherwise return the result as an expression
 *)
let doOp2 (op : op2) (e1 : exp) (e2 : exp) : exp =
	match op, e1, e2 with
		| LT, Const (Int x), Const (Int y) -> Const (Bool (x < y))
		| GT, Const (Int x), Const (Int y) -> Const (Bool (x > y))
		(* Equality takes any 2 constants, of int/bool *)
		| Eq, Const c1, Const c2 -> Const (Bool (c1 = c2))
		| Add, Const (Int x), Const (Int y) -> Const (Int (x + y))
		| Sub, Const (Int x), Const (Int y) -> Const (Int (x - y))
		| Mul, Const (Int x), Const (Int y) -> Const (Int (x * y))
		| Div, Const (Int x), Const (Int y) -> Const (Int (x / y))
		| Mod, Const (Int x), Const (Int y) -> Const (Int (x mod y))
		| _ -> failwith "Attempted op2 application with invalid arguments"

(* TODO Long match on full language, deciding not to use optional args *)
let rec interp (e : exp) (r : env) : exp =
	match e with
		| Id x -> (match lookup x r with
			| Some v -> Const v
			| _ -> failwith "Free identifier"
		)
		| Const c -> Const c
		| Op2 (op, e1, e2) -> doOp2 op (interp e1 r) (interp e2 r)
		| If (e1, e2, e3) -> (match (interp e1 r) with 
			| Const (Bool true) -> interp e2 r
			| Const (Bool false) -> interp e3 r
			| _ -> failwith "If called with non-bool conditional"
		)
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
let _ = print_endline (show_exp (interp (from_file Sys.argv.(1)) [])) *)

(* TODO lots of tests *)
let test_interp_throws ?(r : env = []) (prog : string) : bool =
	try let _ = interp (from_string prog) r in false
	with _ -> true

let test_interp  ?(r : env = []) (prog : string) (res : string) : bool =
	interp (from_string prog) r = from_string res

(* -------ID/Let------- *)
let%TEST "Free identifier is invalid" = test_interp_throws "x" ~r:["y", Int 3]
let%TEST "Bound id is that id's value" = test_interp "x" "3" ~r:["x", Int 3]
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
let%TEST "2 number less than works when true" = test_interp "(0 - 10) < 0" "true"
let%TEST "2 number less than works when false" = test_interp "10 < 0" "false"
let%TEST "2 number greater than works when true" = test_interp "5 > 3" "true"
let%TEST "2 number greater than works when false" = test_interp "(0-12) > (0-2)" "false"
let%TEST "2 number equal works when true" = test_interp "7 == 7" "true"
let%TEST "2 number equal works when false" = test_interp "(0-7) == 7" "false"
let%TEST "Equality for number and bool returns false" = test_interp "1 == true" "false"
let%TEST "Equality for bools works" = test_interp "false == false" "true"

let%TEST "Adding 3 numbers works" = test_interp "3 + 7 + 4" "14"
let%TEST "Order of operations by the structure of expression, not pemdas" = 
	test_interp "(7 + 2) * 0" "0"

let%TEST "Adding bools is invalid" = test_interp_throws "true + false"
let%TEST "Dividing by 0 is invalid" = test_interp_throws "12 / 0"
let%TEST "Modding by 0 is invalid" = test_interp_throws "12 mod 0"

(* ---------If----------- *)
let%TEST "If invalid for non-bool conditional" =
	test_interp_throws "if 1 then 1 else 2"
let%TEST "If evaluates to first expression when true" = test_interp "if true then 1 else 2" "1"
let%TEST "If evaluates to second expression when true" = test_interp "if false then 1 else 2" "2"
let%TEST "If can have an more than a const in its conditional" = test_interp "if (7 > 4) then 1 else 2" "1"

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

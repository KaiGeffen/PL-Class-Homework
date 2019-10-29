(* The second homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/interp.pdf *)

(* Notes 
	To build and run, compile and run main.ml:
	ocamlbuild -use-ocamlfind -pkg compsci631 main.d.byte; ./main.d.byte fileNameHere
	To test:
	ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test test.d.byte; ./test.d.byte
	So far this language is immutable, eg it uses rho but not sigma from the lecture 4
	The language now includes closures (See 5.1 in lecture 4) but not addr/stores/hash-tables
	In interp_util, I think Record should be (id * exp) list instead of (string * exp) list
*)
open Interp_util

(* Env is an environment in which an expression exists
	Which maps any number of named ids to entries (Values or held expressions for Fix)
	The current implementation can have multiple occurence of a given variable,
	but the most recent one will take precedent
 *)
type env = (id * entry) list
(* Values are either constants (int/bool) or closures, and are the only valid return for a program execution
	Closure is a function, with the environment it was defined in, id for its argument, and its contents (expression)
	Functions are first-class members, and are valid returns for a program, can be passed partially applied, etc
 *)
and value = 
(* NOTE(kgeffen) This is a little deceptive, this Const is not an expression (exp), but they look the same *)
	| Const of const
	| Closure of env * id * exp
(* NOTE(kgeffen) These lists aren't typed, and won't be until the type-checker is implemented
	Therefore it is fine to have a list of true::2 for example.
	Also intentionally, a list of lists is fine, a list of closures is also fine
 *)
	| List of value list
	| Record of (id * value) list
	[@@deriving show]
(* An entry in the environment's lookup table *)
and entry = 
	| Value of value
	| HeldExp of exp

(* Lookup the given id in the given list *)
let rec lookup (x : id) (lst : (id * 'a) list) : 'a option =
	match lst with
		| [] -> None
		| (hd_id, hd_v) :: tl -> if x = hd_id then Some (hd_v) else lookup x tl
	
(* Perform the given binary operation with the values
	For invalid applications (eg true + 3), fail
	Otherwise return the result as a value
 *)
let doOp2 (op : op2) (v1 : value) (v2 : value) : value =
	match op, v1, v2 with
		| Eq, _, _ -> Const (Bool (v1 = v2))
		| LT, Const (Int x), Const (Int y) -> Const (Bool (x < y))
		| GT, Const (Int x), Const (Int y) -> Const (Bool (x > y))
		| Add, Const (Int x), Const (Int y) -> Const (Int (x + y))
		| Sub, Const (Int x), Const (Int y) -> Const (Int (x - y))
		| Mul, Const (Int x), Const (Int y) -> Const (Int (x * y))
		| Div, Const (Int x), Const (Int y) -> Const (Int (x / y))
		| Mod, Const (Int x), Const (Int y) -> Const (Int (x mod y))
		| _ -> failwith "Attempted op2 application with invalid arguments"

let rec interp (e : exp) (r : env) : value =
	match e with
		| Id x -> (match lookup x r with
			| Some (Value v) -> v
			| Some (HeldExp e1) -> interp e1 r
			| None -> failwith "Free identifier"
		)
		| Const c -> Const c
		| Op2 (op, e1, e2) -> doOp2 op (interp e1 r) (interp e2 r)
		| If (e1, e2, e3) -> (match (interp e1 r) with 
			| Const (Bool true) -> interp e2 r
			| Const (Bool false) -> interp e3 r
			| _ -> failwith "If called with non-bool conditional"
		)
		(* r' has the new binding at its head, preventing past bindings from taking precedence on lookup *)
		| Let (x, e1, e2) -> interp e2 ((x, Value (interp e1 r)) :: r)
		| Fun (x, e1) -> Closure (r, x, e1)
		| Fix (x, e1) -> interp e1 ((x, HeldExp e1) :: r)
		| App (e1, e2) -> (match (interp e1 r) with
			| Closure (rp, ip, ep) -> interp ep ((ip, Value (interp e2 r)) :: rp)
			| _ -> failwith "Attempted function application on something which isn't a function"
		)
		| Empty -> List []
		| Cons (e1, e2) -> (match e2 with 
			(* Single element list, 1::empty *)
			| Empty -> List [interp e1 r]
			| _ -> (match interp e2 r with
				| List lst -> List ((interp e1 r) :: lst)
				| _ -> failwith "Attempted to cons to something which isn't a list"
			)
		)
		| Head e1 -> (match (interp e1 r) with
			| List (hd :: tl) -> hd
			| _ -> failwith "Attempted to get head of something which isn't a list"
		)
		| Tail e1 -> (match (interp e1 r) with
			| List (hd :: tl) -> List tl
			| _ -> failwith "Attempted to get tail of something which isn't a list"
		)
		| IsEmpty e1 -> Const (Bool ((interp e1 r) = List []))
		(* NOTE(kgeffen) The entries in record are interpreted to values when declared. Not lazy *)
		| Record d -> Record (interp_fields d r)
		| GetField (e1, x) -> (match interp e1 r with
			| Record d -> (match lookup x d with
				| Some v -> v
				| None -> failwith "Attempted to get a field not contained in the given record"
			)  
			| _ -> failwith "Attempted to get field of something which isn't a record"
		)
(* Interpret each field in a given record list *)
and interp_fields (d : (id * exp) list) (r : env) : (id * value) list =
	match d with
		| [] -> []
		| (hd_id, hd_exp) :: tl -> (hd_id, interp hd_exp r) :: interp_fields tl r

(* Base on the third homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/tc.pdf *)
open Tc_util
open Util

(* A type environment is a partial function from variables to types *)
type typeEnv = (id * typ) list

(* Determine the resulting type of the given operations and arguments, or fail if invalid *)
let delta (op : op2) (t1 : typ) (t2 : typ) : typ =
  match op with
    | Eq -> TBool
    | Add | Sub | Mul | Div | Mod -> (match t1, t2 with
      | TInt, TInt -> TInt
      | _ -> failwith "Attempted algebraic operation with non-integers"
    )
    | LT | GT -> (match t1, t2 with
      | TInt, TInt -> TBool
      | _ -> failwith "Attempted to less or greater than with non-integers"
    )

(* Returns the type of the given program, or fails if the given program is not type-correct *)
(* g is the type environment in which the program is considered, stands for Gamma *)
let rec tc (e : exp) (g : typeEnv) : typ =
  match e with 
    | Const Bool _ -> TBool
    | Const Int _ -> TInt
    | Id x -> (match lookup x g with
      | Some t -> t
      | None -> failwith "Free identifier is not type-correct"
    )
    | Let (x, e1, e2) -> tc e2 ((x, tc e1 g) :: g)
    | Op2 (op, e1, e2) -> delta op (tc e1 g) (tc e2 g)
    | If (e1, e2, e3) -> (match (tc e1 g), (tc e2 g) with
      | TBool, t1 -> if (tc e3 g) = t1 then t1 else failwith "The 2 paths in an if have different types"
      | _ -> failwith "If condition was not a bool"
    )
    (* t -> (tc e1 g') *)
    | Fun (x, t, e1) -> TFun (t, (tc e1 ((x, t) :: g)))
    | App (e1, e2) -> (match tc e1 g with
      | TFun (t1, t2) -> if (tc e2 g) = t1 then t2 else failwith "The wrong type of argument was applied to a function"
      | _ -> failwith "Tried to apply to something other than a function"
    )
    | _ -> failwith "not implemented"

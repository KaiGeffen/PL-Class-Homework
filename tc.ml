(* Base on the third homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/tc.pdf *)
open Tc_util

(* A type environment is a partial function from variables to types *)
type typeEnv = (id * typ) list

(* Returns the type of the given program, or fails if the given program is not type-correct *)
(* g is the type environment in which the program is considered, stands for Gamma *)
let rec tc (e : exp) (g : typeEnv) : typ =
  match e with 
    | Const Bool c -> TBool
    | Const Int c -> TInt
    | Id x -> failwith "not implemented"
    | Op2 (op, e1, e2) -> failwith "not implemented"
    | If (e1, e2, e3) -> failwith "not implemented"
    | Fun (x, t, e1) -> failwith "not implemented"
    | App (e1, e2) -> failwith "not implemented"
    | _ -> failwith "not implemented"

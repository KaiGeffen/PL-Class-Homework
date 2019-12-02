(* Base on the fourth homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/verif.pdf *)
(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test -pkg z3 -pkg unix verif.d.byte; ./verif.d.byte FILE_PATH *)
open Smtlib
open Imp
open Printf

(* let z3_path = "/Users/kiv/.opam/4.06.1/lib/z3/" *) (* Update this with the path to Z3 or to a debugging script. *)

(* let solver = make_solver z3_path *)

(* Replace all occurences of x throughout the given body with new_val *)
let rec replace_in_bexp (body : bexp) (x : string) (new_val : aexp) : bexp = 
  match body with
    | BConst c -> BConst c
    | BAnd (b1, b2) -> BAnd (replace_in_bexp b1 x new_val, replace_in_bexp b2 x new_val)
    | BOr (b1, b2) -> BOr (replace_in_bexp b1 x new_val, replace_in_bexp b2 x new_val)
    | BNot b1 -> BNot (replace_in_bexp b1 x new_val)
    | BCmp (cmp1, a1, a2) ->
      BCmp (
        cmp1,
        replace_in_aexp a1 x new_val,
        replace_in_aexp a2 x new_val
      )
and replace_in_aexp (body : aexp) (x : string) (new_val : aexp) : aexp =
  match body with
    | AConst c -> AConst c
    | AVar y -> if x = y then new_val else AVar y
    | AOp (op1, a1, a2) ->
      AOp (
        op1,
        replace_in_aexp a1 x new_val,
        replace_in_aexp a2 x new_val
      )

(* Weakest preconditions *)
let rec wp (c : cmd) (post : bexp) : bexp =
  match c with
  | CSkip -> post
  | CAbort -> BConst false
  | CAssign (x, aexp_val) -> replace_in_bexp post x aexp_val
  | CIf (cond, c1, c2) -> failwith "not implemented"
  | CWhile (b1, b2, c1) -> failwith "not implemented"
  | CSeq (c1, c2) -> failwith "not implemented"

let verify (pre : bexp) (cmd : cmd) (post : bexp) : bool =
  failwith "not implemented"
(* 
let _ =
  let filename = Sys.argv.(1) in
  let (pre, cmd, post) = from_file filename in
  if verify pre cmd post then
    (printf "Verification SUCCEEDED.\n%!"; exit 0)
  else
    (printf "Verification FAILED.\n%!"; exit 1)
 *)
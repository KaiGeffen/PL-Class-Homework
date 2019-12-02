(* Base on the fourth homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/verif.pdf *)
(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test -pkg z3 -pkg unix verif.d.byte; ./verif.d.byte FILE_PATH *)
open Smtlib
open Imp
open Printf

(* let z3_path = "/Users/kiv/.opam/4.06.1/lib/z3/" *) (* Update this with the path to Z3 or to a debugging script. *)

(* let solver = make_solver z3_path *)

(* Weakest preconditions *)
let rec wp (c : cmd) (post : bexp) : bexp =
  match c with
  | CSkip -> post
  | CAbort -> BConst false
  | CAssign (s, a) -> failwith "not implemented"
  | CIf (cond, c1, c2) -> failwith "not implemented"
  | CWhile (b1, b2, c1) -> failwith "not implemented"
  | CSeq (c1, c2) -> failwith "not implemented"

let verify (pre : bexp) (cmd : cmd) (post : bexp) : bool =
  let b = wp cmd post in
  true

let _ =
  let filename = Sys.argv.(1) in
  let (pre, cmd, post) = from_file filename in
  if verify pre cmd post then
    (printf "Verification SUCCEEDED.\n%!"; exit 0)
  else
    (printf "Verification FAILED.\n%!"; exit 1)

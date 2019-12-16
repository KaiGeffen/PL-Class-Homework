(* ocamlbuild -use-ocamlfind -pkg compsci631 main.d.byte; ./main.d.byte JOB FILE *)
open Interp
open Tc
(* open Verif *)

let job = Sys.argv.(1);;
let fp = Sys.argv.(2);;
let _ = 
  if job = "interp" then print_endline (interp_file fp)
  else if job = "tc" then print_endline (tc_file fp)
  (* else if job = "verify" then print_endline (verify ) *)
  else failwith "Unknown job"

(* ocamlbuild -use-ocamlfind -pkg compsci631 main.d.byte; ./main.d.byte JOB FILE *)
open Interp
open Tc
open Tc_util

let job = Sys.argv.(1);;
let fp = Sys.argv.(2);;
let _ = 
  if job = "interp" then print_endline (show_value (interp (from_file fp) []))
  else if job = "tc" then print_endline (show_typ (get_type (from_file fp)))
  else failwith "Unknown job"

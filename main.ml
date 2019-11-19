(* ocamlbuild -use-ocamlfind -pkg compsci631 main.d.byte; ./main.d.byte fileNameHere *)
(* open Interp *)
open Tc
open Tc_util

(* TODO Make Interp use the Tc_util definitions of exp etc so that one utility
  is all that we need to open, and the full language exists for interp, tc, etc
 *)

(* Read the input in the format ./main.d.byte program
  Where program is a filepath to a program in our grammar
  Assume a single line program (No \n, more than 0 chars)
*)
let job = Sys.argv.(1);;
let fp = Sys.argv.(2);;
let _ = 
(*  if job = "interp" then print_endline (show_value (interp (from_file fp) []))
  else *)if job = "tc" then print_endline (show_typ (get_type (from_file fp)))
  else failwith "Unknown job"


(* let _ = print_endline (show_exp (from_file Sys.argv.(1)));;
let _ = print_endline (show_value (interp (from_file Sys.argv.(1)) []))
 *)
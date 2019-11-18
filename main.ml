(* ocamlbuild -use-ocamlfind -pkg compsci631 main.d.byte; ./main.d.byte fileNameHere *)
(* open Interp *)
open Tc_util

(* Read the input in the format ./main.d.byte program
  Where program is a filepath to a program in our grammar
  Assume a single line program (No \n, more than 0 chars)
*)
(* let _ = print_endline (show_exp (from_file Sys.argv.(1)));; *)
(* let _ = print_endline (show_value (interp (from_file Sys.argv.(1)) [])) *)
let _ = print_endline (show_exp (from_file Sys.argv.(1)))
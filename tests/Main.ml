(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test -pkg z3 -pkg unix tests/Main.d.byte; ./Main.d.byte *)
open Interp_tests
open Tc_tests
open Verif_tests

let _ = Ppx_test.Test.collect ()

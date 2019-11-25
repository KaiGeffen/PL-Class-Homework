(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test tests/main.d.byte; ./main.d.byte *)
open Interp_tests
open Tc_tests

let _ = Ppx_test.Test.collect ()

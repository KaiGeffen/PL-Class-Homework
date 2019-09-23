(* The second homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/interp.pdf *)

(* Notes 
	Command slash comments a line
	To build and run:
	ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test main.d.byte; ./main.d.byte
	Ocaml runs everything from top down, main method not necessary
*)

(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

(* TODO Long match on full language *)


(* Read the input in the format
	./interp.d.byte program
	Where program is a filepath to a program in our grammar
*)


(* TODO lots of tests *)


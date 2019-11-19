(* The basic type-checker for our functional language *)
(* Takes a program and returns its type *)
(* Fails if the program has any type-errors *)
open Tc_util

val get_type : exp -> typ

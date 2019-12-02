module PTest = Ppx_test.Test

open Verif
open Imp

(* Helper methods for testing *)
let test_wp (s : string) : bool =
  match from_string s with
    | (pre, c, post) -> pre = (wp c post)

(* TODO these wp tests reflect impl details because the pre / post conditions could look like
  rearrangements, or contain 'and true', etc.
  Reconsider when implementing verif
 *)

let%TEST "Wp for skip is its postcondition" =
  test_wp "requires x == y; ensures x == y; skip;"
let%TEST "Wp for abort is false, no precondition makes abort valid" =
  test_wp "requires false; abort;"
(* let%TEST "Wp for " =
  test_wp "requires true; ensures y==x0 && x==y0; x=x0; y=y0; t=x; x=y; y=t;" *)

(* Examples from slides *)
(* let%TEST "Weakest precondition works for basic assignment program" =
  test_wp "true, x:=x0;" *)
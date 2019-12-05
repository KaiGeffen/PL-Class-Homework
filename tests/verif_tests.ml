module PTest = Ppx_test.Test

open Verif
open Imp

(* NOTE requirement can be impossible and verifier still correctly says success *)

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
let%TEST "Wp for basic assignment works, TODO rework test" =
  test_wp "requires y > 5; ensures y > x; x = 5;"
let%TEST "Wp for basic assignment works CASE 2, TODO rework test" =
  test_wp "requires 2 * y < 5; ensures y < 5; y = 2 * y;"


(* Examples from slides *)
(* let%TEST "Weakest precondition works for basic assignment program" =
  test_wp "true, x:=x0;" *)
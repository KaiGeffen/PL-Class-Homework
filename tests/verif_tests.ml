module PTest = Ppx_test.Test

open Verif
open Imp

(* NOTE requirement can be impossible and verifier still correctly says success *)

(* Helper methods for testing *)
let test_verif (s : string) : bool =
  let (pre, cmd, post) = from_string s in
  verify pre cmd post

(* TODO these wp tests reflect impl details because the pre / post conditions could look like
  rearrangements, or contain 'and true', etc.
  Reconsider when implementing verif
 *)

(* Skip *)
let%TEST "Skip verifies when pre- meets post-" =
  test_verif "requires x == 0; ensures x == 0; skip;"
let%TEST "Skip doesn't verify when pre- sometimes fail to meet post-" =
  not (test_verif "ensures x == 0; skip;")


(* Abort *)

(* assignment *)

(* if *)



(* Seq *)

(* While *)

(* 
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
 *)

(* Examples from slides *)
(* let%TEST "Weakest precondition works for basic assignment program" =
  test_wp "true, x:=x0;" *)
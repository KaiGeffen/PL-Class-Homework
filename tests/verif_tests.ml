module PTest = Ppx_test.Test

open Verif
open Imp

(* NOTE requirement can be impossible and verifier still correctly says success *)

(* Helper methods for testing *)
let test_verif (s : string) : bool =
  let (pre, cmd, post) = from_string s in
  verify pre cmd post

(* Skip *)
let%TEST "Skip verifies when pre- meets post-" =
  test_verif "requires x == 0; ensures x == 0; skip;"
let%TEST "Skip doesn't verify when pre- could fail to meet post-" =
  not (test_verif "ensures x == 0; skip;")

(* Abort *)
let%TEST "Abort doesn't verify in even the simplest situation" =
  not (test_verif "abort;")

(* Assignment *)
let%TEST "Assign verifies when var doesn't appear in post- or pre-" =
  test_verif "x = 5;"
let%TEST "Assign verifies when var appears in post" =
  test_verif "requires y > 6; ensures y > x; x = 5;"
let%TEST "Assign verifies when variable is reassigned" =
  test_verif "ensures x == 2; x = 1; x = 2;"
let%TEST "Assign verifies when variable is assigned to multiple of itself" =
  test_verif "requires x < 0; ensures x <= 6; x = 3 * x;"
let%TEST "Assign verifies when variable referenced but not created in cmd" =
  test_verif "requires x0 == 7; ensures x == 7; x = x0;"
let%TEST "Assign verifies complicated algebra containing multiple vars" =
  test_verif "requires y + z > 7; ensures z > 10; x = y + 3; z = x + z;"

(* If *)
let%TEST "If verifies when conditional is false" =
  test_verif "requires y < 0 && x == 0; ensures x > 0; if (y < 0) x = x + 1; else x = y;"
let%TEST "If verifies when conditional is true" =
  test_verif "requires y > 7; ensures x > 0; if (y < 0) x = x + 1; else x = y;"

(* Seq *)

(* While *)
let%TEST "While verifies incrementing loop with most restrictive invariant" =
  test_verif "requires x <= 10; ensures x == 10; while (x < 10) invariant x <= 10 x = x + 1;"
let%TEST "While verifies incrementing loop with trivial invariant" =
  test_verif "requires x <= 10; ensures x >= 10; while (x < 10) invariant true x = x + 1;"
let%TEST "While fails to verify when loop invariant is violated" =
  not (test_verif "requires x == 11; while (x < 10) invariant x <= 10 x = x + 1;")
let%TEST "While verifies when invariant is false partway through command but true before and after" =
  test_verif "
  requires n0 == 0 && m == 0;
  ensures r == m * n0;
  n = n0;
  r = 0;
  while (n > 0) invariant (m * n0 == r + n * m && n >= 0)
  {
    r = r + m;
    n = n - 1;
  }
  "

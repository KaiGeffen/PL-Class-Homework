module PTest = Ppx_test.Test

open Verif

(* NOTE requirement can be impossible and verifier still correctly says success *)

(* Skip *)
let%TEST "Skip verifies when pre- meets post-" =
  verify_string "requires x == 0; ensures x == 0; skip;"
let%TEST "Skip doesn't verify when pre- could fail to meet post-" =
  not (verify_string "ensures x == 0; skip;")

(* Abort *)
let%TEST "Abort doesn't verify in even the simplest situation" =
  not (verify_string "abort;")

(* Assignment *)
let%TEST "Assign verifies when var doesn't appear in post- or pre-" =
  verify_string "x = 5;"
let%TEST "Assign verifies when var appears in post" =
  verify_string "requires y > 6; ensures y > x; x = 5;"
let%TEST "Assign verifies when variable is reassigned" =
  verify_string "ensures x == 2; x = 1; x = 2;"
let%TEST "Assign verifies when variable is assigned to multiple of itself" =
  verify_string "requires x < 0; ensures x <= 6; x = 3 * x;"
let%TEST "Assign verifies when variable referenced but not created in cmd" =
  verify_string "requires x0 == 7; ensures x == 7; x = x0;"
let%TEST "Assign verifies complicated algebra containing multiple vars" =
  verify_string "requires y + z > 7; ensures z > 10; x = y + 3; z = x + z;"

(* If *)
let%TEST "If verifies when conditional is false" =
  verify_string "requires y < 0 && x == 0; ensures x > 0; if (y < 0) x = x + 1; else x = y;"
let%TEST "If verifies when conditional is true" =
  verify_string "requires y > 7; ensures x > 0; if (y < 0) x = x + 1; else x = y;"

(* Seq *)

(* While *)
let%TEST "While verifies incrementing loop with most restrictive invariant" =
  verify_string "requires x <= 10; ensures x == 10; while (x < 10) invariant x <= 10 x = x + 1;"
let%TEST "While verifies incrementing loop with trivial invariant" =
  verify_string "requires x <= 10; ensures x >= 10; while (x < 10) invariant true x = x + 1;"
let%TEST "While fails to verify when loop invariant is violated" =
  not (verify_string "requires x == 11; while (x < 10) invariant x <= 10 x = x + 1;")
let%TEST "While verifies when invariant is false partway through command but true before and after" =
  verify_string "
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
let%TEST "While verifies loops in sequence" =
  verify_string 
  "requires x <= 10 && y <= 5; ensures x == 10 && y == 5;
  while (x < 10) invariant (x <= 10 && y <= 5) x = x + 1;
  z = 3;
  while (y < 5) invariant (y <= 5 && x == 10) y = y + 1;
  "

let%TEST "While verifies nested loops" =
  verify_string 
  "requires x <= 10 && y == 5; ensures x == 10 && y == 5;
  while (x < 10) invariant (x <= 10 && y == 5)
  {
    x = x + 1;
    while (y < 5) invariant (x <= 10 && y <= 5)
      y = y + 1;
  }
  "

let%TEST "While loop results are only applicable if the loop is reached" =
  verify_string
  "requires x <= 10 && b == 1; ensures x == 10;
  if (b == 1) {while (x < 10) invariant (x <= 10) x = x + 1;}
  else skip;
  "
  && not (verify_string
  "requires x <= 10 && b == 0; ensures x == 10;
  if (b == 1) {while (x < 10) invariant (x <= 10) x = x + 1;}
  else skip;
  "
  )

let%TEST "While loop invariant guarantees must be satisfied even if the loop isn't reachable" =
  not (verify_string "requires z == 5; ensures z == 5;
  if (true) skip;
  else {while (x < 10) invariant (x <= 10) x = x + 1;}
  ") (* Exiting the loop doesn't imply the post condition x == 5 *)

(* The basic verifier for our functional language. *)
(* Takes a program and returns if it verifies *)
(* A program verifies if meeting its preconditions and executing it always guarantees its post-conditions *)
(* Also, all loop invariant guarantees must always be met *)

(* Verify the given program, and return true if it verifies *)
val verify_string : string -> bool

(* Verify the program at the given path, and return a string describing the result *)
val verify_file : string -> string

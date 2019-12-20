(* The basic verifier for our functional language. *)
(* Takes a program and returns if it verifies *)
(* A program verifies if, for every state which meets pre, running cmd will result in a state which meets post *)
(* Also, all loop invariant guarantees must always be met *)

(* Verify the given program, and return true if it verifies *)
val verify_string : string -> bool

(* Verify the program at the given path, and return a string describing the result *)
val verify_file : string -> string

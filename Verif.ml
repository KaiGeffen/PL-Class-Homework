(* Base on the fourth homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/verif.pdf *)
(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test -pkg z3 -pkg unix verif.d.byte; ./verif.d.byte FILE_PATH *)
open Smtlib
open Imp
open Printf

(* TODO Put this in an external file which centralizes these settings *)
let z3_path = "/usr/local/Cellar/z3/4.8.7/bin/z3" (* Update this with the path to Z3 or to a debugging script. *)

let solver = make_solver z3_path

(* Replace all occurences of x throughout the given body with new_val *)
let rec replace_in_bexp (body : bexp) (x : string) (new_val : aexp) : bexp = 
  match body with
    | BConst c -> BConst c
    | BAnd (b1, b2) -> BAnd (replace_in_bexp b1 x new_val, replace_in_bexp b2 x new_val)
    | BOr (b1, b2) -> BOr (replace_in_bexp b1 x new_val, replace_in_bexp b2 x new_val)
    | BNot b1 -> BNot (replace_in_bexp b1 x new_val)
    | BCmp (cmp1, a1, a2) ->
      BCmp (
        cmp1,
        replace_in_aexp a1 x new_val,
        replace_in_aexp a2 x new_val
      )
and replace_in_aexp (body : aexp) (x : string) (new_val : aexp) : aexp =
  match body with
    | AConst c -> AConst c
    | AVar y -> if x = y then new_val else AVar y
    | AOp (op1, a1, a2) ->
      AOp (
        op1,
        replace_in_aexp a1 x new_val,
        replace_in_aexp a2 x new_val
      )

(* Find the weakest precondition for which running cmd will necessarily meet the given post-condition
  Return the weakest precondition, followed by the conjunction of each guarantee about the loop-invariant
  The guarantee, for each loop, is that the post-condition is met on exiting the loop,
  and the invariant is preserved each iteration which doesn't exit the loop
*)
let rec wp (c : cmd) (post : bexp) : bexp * bexp =
  match c with
    | CSkip -> post, BConst true
    | CAbort -> BConst false, BConst true
    | CAssign (x, aexp_val) -> replace_in_bexp post x aexp_val, BConst true
    (* b -> wp1 & !b -> wp2 *)
    (* (!b or wp1) & (b or wp2) *)
    | CIf (b, c1, c2) ->
      let wp1, g1 = wp c1 post in
      let wp2, g2 = wp c2 post in
      BAnd (
        BOr (BNot b, wp1),
        BOr (b, wp2)
      ), BAnd (g1, g2)
    | CWhile (b, i, c1) -> 
      (* On exit (When b is false) statement must be valid to satisfy invariant guarantee *)
      (* (!b & I) -> Q  => !(!b & I) or Q *)
      let exit_guar : bexp = BOr (BNot (BAnd (BNot b, i)), post) in
      (* After each iteration of the loop, the invariant must be reestablished to satisfy invariant guarantee *)
      (* (b & I) -> wp (c, I)  =>  !(b & I) or wp (c, I) *)
      (* Any guarantees within this loop (Nested loops) must also be valid *)
      let wpi, g1 = wp c1 i in
      let loop_guar : bexp = BOr (BNot (BAnd (b, i)), wpi) in
      i, BAnd (g1, BAnd (loop_guar, exit_guar))
    | CSeq (c1, c2) -> 
      let wp2, g2 = wp c2 post in
      let wp1, g1 = wp c1 wp2 in
      wp1, BAnd (g1, g2)

(* Declare the given identifier, if it hasn't been declared already *)
(* This method returns nothing meaningful, but has the side-effect of adding to the solver *)
let declare_const (x : string) : unit =
  try Smtlib.declare_const solver (Id x) int_sort
  with _ -> ()

(* Transform the predicates in body into z3 terms *)
let rec bexp_to_term (body : bexp) : Smtlib.term =
  match body with
    | BConst b -> Smtlib.bool_to_term b
    | BAnd (b1, b2) -> Smtlib.and_ (bexp_to_term b1) (bexp_to_term b2)
    | BOr (b1, b2) -> Smtlib.or_ (bexp_to_term b1) (bexp_to_term b2) 
    | BNot b1 -> Smtlib.not_ (bexp_to_term b1)
    | BCmp (cmp1, a1, a2) -> (match cmp1 with
      | Lt -> Smtlib.lt
      | Gt -> Smtlib.gt
      | Eq -> Smtlib.equals
      | Lte -> Smtlib.lte
      | Gte -> Smtlib.gte
    ) (aexp_to_term a1) (aexp_to_term a2)
and aexp_to_term (body : aexp) : Smtlib.term =
  match body with
    | AConst i -> Smtlib.int_to_term i
    (* NOTE(kgeffen) Declaring constants here is a side-effect of this method *)
    | AVar x -> declare_const x; Smtlib.const x
    | AOp (op1, a1, a2) -> (match op1 with
      | Add -> Smtlib.add
      | Sub -> Smtlib.sub
      | Mul -> Smtlib.mul
    ) (aexp_to_term a1) (aexp_to_term a2)

(* Return true if for every state which meets pre, running cmd will result in a state which meets post *)
let _ = Smtlib.push solver
let verify (pre : bexp) (c : cmd) (post : bexp) : bool =
  (* Necessary for tests to work *)
  let _ = Smtlib.pop solver in
  let _ = Smtlib.push solver in

  let wp1, guarantees = wp c post in

  (* Check that loop invariant guarantees are met *)
  (* Valid when !guarantees is UNSAT *)
  Smtlib.push solver;
  Smtlib.assert_ solver (Smtlib.not_ (bexp_to_term guarantees));
  let guarantees_met : bool = check_sat solver = Unsat in
  Smtlib.pop solver;

  (* (Pre implies weakest_pre) should be valid *)
  (* Valid when !(pre -> wp) is UNSAT *)
  let formula = (
    Smtlib.not_ (Smtlib.implies (bexp_to_term pre) (bexp_to_term wp1))
  ) in
  Smtlib.assert_ solver formula;
  let pre_implies_wp = check_sat solver = Unsat in
  
  guarantees_met && pre_implies_wp

(* NOTE This exists and is exposed for testing verif *)
let verify_string (s : string) : bool =
  let (pre, cmd, post) = from_string s in
  verify pre cmd post

(* Verify the program at the given path, and return a string describing the result *)
let verify_file (fp : string) : string =
  let (pre, cmd, post) = from_file fp in
  if verify pre cmd post then
    "Verification SUCCEEDED!"
  else
    "Verification FAILED!"

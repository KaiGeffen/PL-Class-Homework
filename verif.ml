(* Base on the fourth homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/verif.pdf *)
(* ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test -pkg z3 -pkg unix verif.d.byte; ./verif.d.byte FILE_PATH *)
open Smtlib
open Imp
open Printf

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

(* Weakest preconditions *)
let rec wp (c : cmd) (post : bexp) : bexp =
  match c with
    | CSkip -> post
    | CAbort -> BConst false
    | CAssign (x, aexp_val) -> replace_in_bexp post x aexp_val
    (* b -> wp1 & !b -> wp2 *)
    (* (!b or wp1) & (b or wp2) *)
    | CIf (b, c1, c2) ->
      BAnd (
        BOr (BNot b, wp c1 post),
        BOr (b, wp c2 post)
      )
    | CWhile (b1, invariant, c1) -> failwith "not implemented"
    | CSeq (c1, c2) -> wp c1 (wp c2 post)

(* Declare the given identifier, or do nothing if it's already declared *)
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

let verify (pre : bexp) (c : cmd) (post : bexp) : bool =
  let pre_term = bexp_to_term pre in
  let wp_term = bexp_to_term (wp c post) in

  (* Pre implies weakest_pre should be _valid_ *)
  (* Valid when !(pre -> wp) is UNSAT *)
  let formula = (
    Smtlib.not_ (Smtlib.implies pre_term wp_term)
  ) in
  let _ = Smtlib.assert_ solver formula in
  let result = check_sat solver = Unsat in
  (* TEMP *)
  (* let lst : (identifier * term) list = get_model solver in
  let rec print_list (l : (identifier * term) list) = 
    match l with
      | [] -> ()
      | (x,v) :: t -> printf "%S : %S\n" "x hehe" (sexp_to_string (term_to_sexp v)); print_list t; in
  let _ = print_list lst in *)
  result

let _ =
  let filename = Sys.argv.(1) in
  let (pre, cmd, post) = from_file filename in
  if verify pre cmd post then
    (printf "Verification SUCCEEDED.\n%!"; exit 0)
  else
    (printf "Verification FAILED.\n%!"; exit 1)
 
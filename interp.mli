(* The basic interpreter for our functional language *)
open Tc_util

type env = (id * entry) list
and value = 
  | Const of const
  | Closure of env * id * exp
  | List of value list
  | Record of (id * value) list
  | Array of value array
  [@@deriving show]
and entry =
  | Value of value
  | HeldExp of exp

val interp : exp -> env -> value

open Printf
open Compiler_util2

let op_exp_to_assembly (op : op2) : ILVM.op2 =
  match op with
    | LT -> ILVM.LT
    | GT -> failwith "TODO assembly does not support greater than"
    | Eq -> ILVM.Eq
    | Add -> ILVM.Add
    | Sub -> ILVM.Sub
    | Mul -> ILVM.Mul
    | Div -> ILVM.Div
    | Mod -> ILVM.Mod

let fresh_id : unit -> id = 
  let n = ref 0 in
  fun _ -> 
    let x = "tmp" ^ string_of_int !n in
    n := !n + 1;
    x

(* TODO better comments *)
(* Convert an expression in the main language to Continuation Passing Style *)
(* The return type is from basic function to expression *)
(* Basically from k (a continuation frame) to the final result *)
let rec cps (e : exp) : (exp -> exp) -> exp =
  match e with
    | Const (Int n) -> fun k -> k (Const (Int n))
    | Const (Bool b) -> fun k -> k (Const (Bool b))
    | Id x -> fun k -> k (Id x)
    | Op2 (Add, e1, e2) -> fun k ->
      cps (e1) (fun x1 ->
        cps (e2) (fun x2 ->
          let r = fresh_id () in
          Let (r, Op2 (Add, x1, x2), k (Id r))
        )
      )
    | _ -> failwith "not implemented"


(* let rec exp_to_instr (e : exp) (next_instr : ILVM.instr) : ILVM.instr = *)
(*  match e with
  | Id x -> failwith "not implemented"
  | Const (Int i) -> ILVM.Store (0, Imm i, next_instr)
  | Const c -> failwith "not implemented"
  | Op2 (op, e1, e2) -> 
    exp_to_instr ()
    Op2 (
      0,
      op_exp_to_assembly op,
      ILVM.Reg 1,
      ILVM.Reg 2,
      next_instr
    )
  | App (ef, e_args) -> failwith "not implemented"
  | If (e1, e2, e3) -> failwith "not implemented"
  | Let (x, e1, e2) -> failwith "not implemented"
  | Fun (f_name, arg_ids, body) -> failwith "not implemented"
  | MkArray (e1, e2) -> failwith "not implemented"
  | GetArray (e1, e2) -> failwith "not implemented"
  | SetArray (e1, e2, e3) -> failwith "not implemented"
  | Seq (e1, e2) -> failwith "not implemented"
  | Abort -> failwith "not implemented" *)

(* Convert an expression in cps form into assembly *)
let rec cps_to_assembly (e : exp) : ILVM.instr =
  match e with
    | Const (Int n) -> ILVM.Exit (ILVM.Imm n)
    | Let (x, v, e1) -> cps_to_assembly (subst x v e1)
    | Op2 (Add, e1, e2) -> (match (eval e1, eval e2) with
      | Int v1, Int v2 -> ILVM.Op2 (0,
        ILVM.Add, 
        ILVM.Imm v1,
        ILVM.Imm v2,
        ILVM.Exit (ILVM.Reg 0))
      | _ -> failwith "didnt eval to int")
    | _ -> failwith "not implemented"

(* Convert an expression from the main language into assembly *)
let rec exp_to_assembly (e : exp) : ILVM.block list =
  print_endline (show_exp (cps e (fun x -> x)));
  [0, cps_to_assembly (cps e (fun x -> x))]

let _ =
  let filename : string = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let program : exp = from_file filename in
  let assembly : ILVM.block list = exp_to_assembly program in
  let out_chan = open_out out_filename in
  output_string out_chan (ILVM.string_of_blocks assembly);
  close_out out_chan

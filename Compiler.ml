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

let rec exp_to_assembly (e : exp) : ILVM.block list =
  match e with
  | Id x -> failwith "not implemented"
  | Const c -> failwith "not implemented"
  | Op2 (op, e1, e2) -> failwith "not implemented"
    (* Op2 (
      reg,
      op_exp_to_assembly op,
      val1,
      val2,
      Exit reg_value
    ) *)
  | App (ef, e_args) -> failwith "not implemented"
  | If (e1, e2, e3) -> failwith "not implemented"
  | Let (x, e1, e2) -> failwith "not implemented"
  | Fun (f_name, arg_ids, body) -> failwith "not implemented"
  | MkArray (e1, e2) -> failwith "not implemented"
  | GetArray (e1, e2) -> failwith "not implemented"
  | SetArray (e1, e2, e3) -> failwith "not implemented"
  | Seq (e1, e2) -> failwith "not implemented"
  | Abort -> failwith "not implemented"

let _ =
  let filename : string = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let program : exp = from_file filename in
  let assembly : ILVM.block list = exp_to_assembly program in
  let out_chan = open_out out_filename in
  output_string out_chan (ILVM.string_of_blocks assembly);
  close_out out_chan

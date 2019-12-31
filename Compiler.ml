open Printf
open Compiler_util2

let rec exp_to_assembly (e : exp) : ILVM.block list =
  match e with
  | _ -> failwith "not implemented"

let _ =
  let filename : string = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let program : exp = from_file filename in
  let assembly : ILVM.block list = exp_to_assembly program in
  let out_chan = open_out out_filename in
  output_string out_chan (ILVM.string_of_blocks assembly);
  close_out out_chan

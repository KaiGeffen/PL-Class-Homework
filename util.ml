(* Utility methods used throughout multiple files *)
open Interp_util

(* Lookup the given id in the given list *)
let rec lookup (x : id) (lst : (id * 'a) list) : 'a option =
  match lst with
    | [] -> None
    | (hd_id, hd_v) :: tl -> if x = hd_id then Some (hd_v) else lookup x tl

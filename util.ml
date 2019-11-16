(* Utility methods used throughout multiple files *)
open Interp_util

(* Lookup the given id in the given list *)
let rec lookup (x : id) (lst : (id * 'a) list) : 'a option =
  match lst with
    | [] -> None
    | (hd_id, hd_v) :: tl -> if x = hd_id then Some (hd_v) else lookup x tl

(* Returns true if the given element is in the list, false otherwise *)
let rec contains (x : 'a) (lst : 'a list) : bool =
  match lst with
    | [] -> false
    | hd :: tl -> if x = hd then true else contains x tl

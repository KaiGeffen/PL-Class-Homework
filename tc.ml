(* The Third homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/tc.pdf *)

let rec tc (e : exp) : bool =
  match e with 
  | _ -> failwith "operation not defined"

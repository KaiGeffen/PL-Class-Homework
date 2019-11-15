(* Base on the third homework from Arjun Guha's (UMass) Fall 2018 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2018/hw/tc.pdf *)
open Tc_util
open Util

(* Environment is a partial function from variables to types *)
type env = (id * typ) list

(* Type environment is the set of all defined type ids *)
type typEnv = id list

(* Determine the resulting type of the given operations and arguments, or fail if invalid *)
let checkOp (op : op2) (t1 : typ) (t2 : typ) : typ =
  match op with
    | Eq -> TBool
    | Add | Sub | Mul | Div | Mod -> (match t1, t2 with
      | TInt, TInt -> TInt
      | _ -> failwith "Attempted algebraic operation with non-integers"
    )
    | LT | GT -> (match t1, t2 with
      | TInt, TInt -> TBool
      | _ -> failwith "Attempted to less or greater than with non-integers"
    )

(* Returns the type of the given program, or fails if the given program is not type-correct *)
(* g (Gamma) : environment, maps bound variables to their types *)
(* d (Delta) : type environment, is the set of type ids which are defined *)
(* TODO order these cases, consistent with interp and other files *)
let rec tc (e : exp) (g : env) (d : typEnv) : typ =
  match e with 
    | Const Bool _ -> TBool
    | Const Int _ -> TInt
    | Id x -> (match lookup x g with
      | Some t -> t
      | None -> failwith "Free identifier is not type-correct"
    )
    | Let (x, e1, e2) -> tc e2 ((x, tc e1 g d) :: g) d
    | Op2 (op, e1, e2) -> checkOp op (tc e1 g d) (tc e2 g d)
    | If (e1, e2, e3) -> (match (tc e1 g d), (tc e2 g d) with
      | TBool, t1 -> if (tc e3 g d) = t1 then t1 else failwith "The 2 paths in an if have different types"
      | _ -> failwith "If condition was not a bool"
    )
    (* t -> (tc e1 g') *)
    | Fun (x, t, e1) -> TFun (t, (tc e1 ((x, t) :: g) d))
    | App (e1, e2) -> (match tc e1 g d with
      | TFun (t1, t2) -> if (tc e2 g d) = t1 then t2 else failwith "The wrong type of argument was applied to a function"
      | _ -> failwith "Tried to apply to something other than a function"
    )
    | Fix (x, t, e1) -> if tc e1 ((x, t) :: g) d = t then t else failwith "Fix type did not match the type given"
    | Empty t -> TList t
    | IsEmpty e -> TBool
    | Cons (e1, e2) -> (
      let (t1, t2) = (tc e1 g d, tc e2 g d) in
      (* If either it's 2 elements of the same type, or an element followed by a list of its type *)
      if t1 = t2 || TList t1 = t2 then TList t1
      else failwith "The elements of a list must be homogenous"
    )
    | Head e1 -> (match tc e1 g d with
      | TList t -> t
      | _ -> failwith "Tried to get head of something which isn't a list"
    )
    | Tail e1 -> (match tc e1 g d with
      | TList t -> TList t
      | _ -> failwith "Tried to get tail of something which isn't a list"
    )
    | MkArray (e1, e2) -> (match tc e1 g d with
      | TInt -> TArr (tc e2 g d)
      | _ -> failwith "Array length must be an int"
    )
    | GetArray (e1, e2) -> (match tc e2 g d with
      | TInt -> (match tc e1 g d with
        | TArr t -> t
        | _ -> failwith "GetArray called on something which isn't an array"
      )
      | _ -> failwith "GetArray index must be an int"
    )
    | SetArray (e1, e2, e3) -> (match tc e2 g d with
      | TInt -> (match tc e1 g d with
        | TArr t -> if tc e3 g d = t then t else failwith "Attempted to set an array element to a type which that array isn't"
        | _ -> failwith "SetArray called on something which isn't an array"
      )
      | _ -> failwith "SetArray index must be an int"
    )
    | TypFun (a, e1) -> TForall (a, tc e1 g (a :: d))
    | _ -> failwith "not implemented"

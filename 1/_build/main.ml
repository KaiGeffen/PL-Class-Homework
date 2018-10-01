(* The first homework from Arjun Guha's (UMass) Fall 2017 CS631 'Programming-Languages' class *)
(* https://people.cs.umass.edu/~arjun/courses/compsci631-fall2017/hw/fp.ml *)
(* Answered by Kai Geffen September 2018 *)

(* To compile this file, run:
 *
 * ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test fp.d.byte
 *
 * This produces an executable called ./fp.d.byte, which runs all the
 * test cases in the file.
 *
 * If you've renamed the file to say foo.ml, you can instead run:
 *
 * ocamlbuild -use-ocamlfind -pkg compsci631 -pkg ppx_test foo.d.byte
 *)

(* This is necessary for let%TEST to work. *)
module PTest = Ppx_test.Test

(* Do not change this definition *)
let rec foldr (f : 'a -> 'b -> 'b) (acc: 'b) (lst : 'a list) : 'b =
  match lst with
  | [] -> acc
  | hd :: tl -> f hd (foldr f acc tl)

(* Do not change this definition *)
let rec unfold (p : 'b -> bool) (g : 'b -> 'a * 'b) (acc : 'b) : 'a list =
  match p acc with
    | true -> []
    | false ->
      let (a, acc') = g acc in
      a :: (unfold p g acc')

let length (lst : 'a list) : int =
  foldr (fun _ y -> 1 + y) 0 lst

let%TEST "[length] length of empty list is 0" = length [] = 0
let%TEST "[length] length of single element list is 1" = length [true] = 1
let%TEST "[length] length of multi-element list is correct" = length [2; 3; 10] = 3

let filter (pred : 'a -> bool) (lst : 'a list) : 'a list =
  foldr (fun x y -> if pred x then x :: y else y) [] lst

let is_even (x : int) : bool = x mod 2 = 0
let%TEST "[filter] filtering an empty list returns empty list" = filter is_even [] = []
let%TEST "[filter] filtering a single element list returns that list if it satisfies" = filter is_even [8] = [8]
let%TEST "[filter] filtering a single element list returns empty list if it doesn't satisfy" = filter is_even [9] = []
let%TEST "[filter] filtering a multi-element works correctly" = filter is_even [8;10;3;3;4;4;1] = [8;10;4;4]

let build_list (f : int -> 'a) (len : int) : 'a list =
  unfold (fun x -> x = len) (fun x -> (f x, x + 1)) 0

let%TEST "[build_list] building a length 0 list yields empty list" = build_list is_even 0 = []
let%TEST "[build_list] building a length 1 list yields a correct single element list" = build_list is_even 1 = [true]
let%TEST "[build_list] building a length > 1 list yields a correct multi-element list" = build_list is_even 6 = [true; false; true; false; true; false]

let is_empty (lst : 'a list) : bool =
  length lst = 0

let%TEST "[is_empty] test on empty list is true" = is_empty [] = true
let%TEST "[is_empty] test on single element 0 list is false" = is_empty [0] = false
let%TEST "[is_empty] test on multi-element list is false" = is_empty ["here";"hey";"welcome"] = false

let zip (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  let p (lst_pair : 'a list * 'b list) : bool = is_empty (fst lst_pair) || is_empty (snd lst_pair) in
  let g (lst_pair : 'a list * 'b list) : (('a * 'b) * ('a list * 'b list)) = match lst_pair with
    | (hd1 :: tl1, hd2 :: tl2) -> ((hd1, hd2), (tl1, tl2))
    | _ -> failwith "Shouldn't see this because predicate guarantees list isn't empty" in
  unfold p g (lst1, lst2)

let%TEST "[zip] zipping empty lists yields empty list" = zip [] [] = []
let%TEST "[zip] zipping empty list and unempty list yields empty list" = zip [2;2;1] [] = []
let%TEST "[zip] zipping 2 single element lists works correctly" = zip ["a"] [2] = [("a",2)]
let%TEST "[zip] zipping equal length multi-element lists works correctly" = zip ["a";"b";"d"] [2;0;40] = [("a",2);("b",0);("d",40)]
let%TEST "[zip] zipping unequal length multi-element lists omits the extras from longer list" = zip ["a";"b";"d"] [2;0;40;50;60;70] = [("a",2);("b",0);("d",40)]

let map_using_unfold (f : 'a -> 'b) (lst : 'a list) : 'b list =
  let g (l : 'a list) : 'b * 'a list = match l with
    | hd :: tl -> (f hd, tl)
    | _ -> failwith "shouldn't see this because predicate guarantees list isn't empty" in 
  unfold (fun alst -> is_empty alst) g lst

let%TEST "[map_using_unfold] mapping an empty list yields an empty list" = map_using_unfold (String.length) [] = []
let%TEST "[map_using_unfold] mapping a single element list works" = map_using_unfold (String.length) ["hello"] = [5]
let%TEST "[map_using_unfold] mapping a multi-element list works" = map_using_unfold (String.length) ["hello"; "world"; "goodbye"; "flies"] = [5; 5; 7; 5]

let map_using_fold (f : 'a -> 'b) (lst : 'a list) : 'b list =
  let foldr_f (x : 'a) (rst : 'b list) : 'b list = f x :: rst in
  foldr foldr_f [] lst

let%TEST "[map_using_fold] mapping an empty list yields an empty list" = map_using_fold (String.length) [] = []
let%TEST "[map_using_fold] mapping a single element list works" = map_using_fold (String.length) ["hello"] = [5]
let%TEST "[map_using_fold] mapping a multi-element list works" = map_using_fold (String.length) ["hello"; "world"; "goodbye"; "flies"] = [5; 5; 7; 5]

(* NOTE(kgeffen) This method works only for non-negative whole numbers *)
let factorial n =
  foldr (fun x y -> x * y) 1 (build_list (fun x -> x + 1) n)

let%TEST "[factorial] factorial 0 is 1" = factorial 0 = 1
let%TEST "[factorial] factorial 1 is 1" = factorial 1 = 1
let%TEST "[factorial] factorial 5 is 120" = factorial 5 = 120

let insert (x : int) (lst : int list) : int list =
  (* Function takes an element of the list it's traversing and the rest of the list that will be made by foldr
  If x should lie between n and the head of that list, insert it as well
  This should only occur once, since it checks for a strictly larger predecessor *)
  let f (n : int) (lst_rem : int list) : int list = match lst_rem with
    | hd :: tl -> if (hd > x && n <= x) then n :: x :: lst_rem else n :: lst_rem
    | [] -> if n <= x then [n;x] else [n] in

  (* Here we have to handle 2 special cases: That lst is empty, and that x is less than all elements of lst *)
  (* Essentially both these cases are where x should be the very head of the resultant list *)
  match lst with
    | hd :: tl -> if hd > x then x :: lst else foldr f [] lst
    | [] -> [x]

let%TEST "[insert] inserting into empty list yields list with just that element" = insert 3 [] = [3]
let%TEST "[insert] inserting into single-element list works when inserting a smaller number" = insert 2 [4] = [2;4]
let%TEST "[insert] inserting into single-element list works when inserting a larger number" = insert 6 [4] = [4;6]
let%TEST "[insert] inserting into single-element list works when inserting an equal number" = insert 4 [4] = [4;4]
let%TEST "[insert] inserting into multi-element list with multiples of the added element works" = insert 7 [1;7;7;7] = [1;7;7;7;7]
let%TEST "[insert] inserting into multi-element list works when inserting into the middle" = insert 7 [1;2;3;4;5;6;8;9] = [1;2;3;4;5;6;7;8;9]

let insertion_sort (xs : int list) =
  let f (n : int) (ns : int list) : int list = insert n ns in
  foldr f [] xs

let%TEST "[insertion_sort] empty list yields empty list" = insertion_sort [] = []
let%TEST "[insertion_sort] single-element list yields that same list" = insertion_sort [3] = [3]
let%TEST "[insertion_sort] list of all the same element yields that same list" = insertion_sort [4;4;4] = [4;4;4]
let%TEST "[insertion_sort] sorted list yields itself" = insertion_sort [0;4;14] = [0;4;14]
let%TEST "[insertion_sort] unsorted list with unique elements correctly sorts" = insertion_sort [2;4;3;1] = [1;2;3;4]
let%TEST "[insertion_sort] unsorted list with some non-unique elements correctly sorts" = insertion_sort [3;1;5;1;8;4;8;4;4] = [1;1;3;4;4;4;5;8;8]

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

(* Traverse both trees in order at most once *)
(* If the nth pair of elements are different, should not examine the (n + 1)th pair of elements *)
(* I remember that people advocated for forming one tree into a list, then popping it as you checked the other tree *)
(* That solution is... eh? I think it meets the specs *)
(* It think it would be more interesting to be on the order of the most efficient solution *)
(* Although this question is O(n) no matter what, so it really is just a lesser efficiency question isn't it. *)
(* TODO(kgeffen) Consider what specs this must meet, then implement *)
let rec same_in_order (t1 : 'a tree) (t2 : 'a tree): bool =
  failwith "not implemented"

let%TEST "[tree.same_in_order] empty trees are the same" = same_in_order Leaf Leaf = true
let%TEST "[tree.same_in_order] empty tree is not the same as single node tree" = same_in_order Leaf (Node (Leaf, 3, Leaf)) = false
let%TEST "[tree.same_in_order] 2 single node trees with unequal elements are not the same" = same_in_order (Node (Leaf, 5, Leaf)) (Node (Leaf, 3, Leaf)) = false
let%TEST "[tree.same_in_order] 2 single node trees with equal elements are the same" = same_in_order (Node (Leaf, 5, Leaf)) (Node (Leaf, 5, Leaf)) = true

let t1_ex = (Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)))
let t2_ex = (Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf))
let%TEST "[tree.same_in_order] multi-element positive example" = same_in_order t1_ex t2_ex = true
let t3_ex = (Node (Node (Node (Leaf, 1200, Leaf), 2, Leaf), 3, Leaf))
let%TEST "[tree.same_in_order] multi-element negative example" = same_in_order t1_ex t3_ex = false

(* Formed from high to low because it makes list generation a bit easier *)
let rec from_to_helper (m : int) (n : int) (acc : int list) : int list = 
  if m = n then n :: acc
  else from_to_helper m (n - 1) (n :: acc)

let rec from_to (m : int) (n: int) : int list =
  if m > n then failwith "from_to given larger number as lower bound"
  else from_to_helper m n []

let%TEST "[from_to] n to n yields a single element list [n]" = from_to 3 3 = [3]
let%TEST "[from_to] small difference between m and n works as expected" = from_to 1 6 = [1;2;3;4;5;6]
(* TODO(kgeffen) Make this explicitly test if the resources are exhausted and not just implicitly test that *)
let%TEST "[from_to] large difference between m and n doesn't fail to run because of lack of resources" = from_to 1 1000000 != []

let%TEST "unsurprisingly, addition works correctly" = 1 + 2 = 3

(* Runs all tests declared with let%TEST. This must be the last line
   in the file. *)
let _ = Ppx_test.Test.collect ()

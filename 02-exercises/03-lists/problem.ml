open! Base
open Expect_test_helpers_kernel

(*
   OCaml natively supports linked lists as part of the language.
   Lists are commonly referred to as having heads and tails.
   The head is the first element of the linked list
   The tail is everything else.

   [] means "the empty list".
   hd :: tl means "the element hd added to the front of the list tl".

   When matching on a list, it's either empty or non-empty. To say it another way, it's
   either equal to [] or equal to (hd :: tl) where hd is the first element of the list
   and tl] is all the rest of the elements of the list (which may itself be empty).
*)

(* This function computes the length of a list. *)
let rec length list =
  match list with
  | [] -> 0
  | _ :: tl -> 1 + (length tl)

(* EXERCISE: Uncomment this example and fill it out! *)
(*
(** [sum list] returns the sum of all integers on [list] *)
let rec sum list =
  failwith "For you to implement"

let%test_unit "sum" =
  [%test_eq: int] 0  (sum []);
  [%test_eq: int] 55 (sum [55]);
  [%test_eq: int] 0  (sum [5 ; -5 ; 1 ; -1])
*)

(* EXERCISE: Uncomment this example and fill it out! *)

(** [range lower upper] returns in sorted order all integers greater
   than or equal to [lower] and strictly less than [upper] *)
let rec range l u =
  if l >= u then []
  else l :: range (l + 1) u

let%test_unit "range" =
  let test l u expected = [%test_eq: int list] (range l u) expected in
  test 1 4 [1;2;3];
  test (-5) 3 [-5;-4;-3;-2;-1;0;1;2];
  test 10 0 []

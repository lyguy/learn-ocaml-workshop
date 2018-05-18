open! Base
open Expect_test_helpers_kernel

(* Singly-linked lists are a common enough data type that they have
   some built-in syntactic support.

   In particular, [] means "the empty list", and hd :: tl means "the
   element hd added to the front of the list tl".
*)

(* In this function, we use [] and :: in a pattern match to break down
   a list. Note that we need the [rec] keyword to mark the function as
   recursive. *)
let rec length list =
  match list with
  | [] -> 0
  | _ :: tail -> 1 + (length tail)

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

(* In this example, we also use [] and :: for constructing new list. *)
let rec double_elements list =
  match list with
  | [] -> []
  | head :: tail -> head * 2 :: double_elements tail

(* EXERCISE: Uncomment this example and fill it out! *)
(*
(** [range lower upper] returns in sorted order all integers greater
   than or equal to [lower] and strictly less than [upper] *)
let rec range l u =
  failwith "unimplemented"

let%test_unit "range" =
  [%test_eq: int list] (range 1 4)    [1;2;3];
  [%test_eq: int list] (range (-5) 3) [-5;-4;-3;-2;-1;0;1;2];
  [%test_eq: int list] (range 10 0)   []
*)

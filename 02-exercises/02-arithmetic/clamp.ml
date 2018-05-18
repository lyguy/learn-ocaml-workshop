open! Base

(* Implement the [clamp] function below, correctly!

   You'll need to use OCaml's if statement:

   {[
     if cond
     then expr1
     else expr2
   ]}

   As well as OCaml's infix comparison operators, e.g., [<].
 *)
(** [clamp l h x] always returns a number between [l] and [h],
    returning [x] if it is between the bounds. If it's outside of one
    of the bounds, return that bound. *)
let clamp _low _high x =
  x
  (* We put underscores before _low and _high so OCaml doesn't warn us
     about unused variables. You should remove them as you try to
     solve the problem. *)

(* These tests use OCaml's inline test framework. Uncomment them when
   you're ready to test your code *)
(*
let%test "low clamp"  = clamp 3 10 0 = 3
let%test "high clamp" = clamp 3 10 20 = 10
let%test "unclamped"  = clamp 3 10 5 = 5
*)

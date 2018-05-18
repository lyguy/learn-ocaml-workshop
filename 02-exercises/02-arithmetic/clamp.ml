open! Base
open! Stdio

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
let%test "unclamped"  = clamp 3 10 5 = 5
(*
let%test "low clamp"  = clamp 3 10 0 = 3
let%test "high clamp" = clamp 3 10 20 = 10
*)

(* Now, let's implement Clamp on floating point numbers. You'll need
   to refer to the floating point comparison functions, by writing
   things like:

   {[ Float.(>) x y ]}

   Or

   {[ Float.(x > y) ]}
*)

(** [clamp l h x] always returns a number between [l] and [h],
    returning [x] if it is between the bounds. If it's outside of one
    of the bounds, return that bound. *)
let clamp_float _low _high x =
  x

(* Uncomment the tests when you're ready to go!

   Note that these tests use [%test_eq], which is a macro that
   generates a comparison function that generates a useful error
   message, printing out the values that don't match. *)
let%test_unit "unclamped"  = [%test_eq: float] (clamp_float 3. 10. 5.) 5.
(*
let%test_unit "low clamp"  = [%test_eq: float] (clamp_float 3. 10. 0.) 3.
let%test_unit "high clamp" = [%test_eq: float] (clamp_float 3. 10. 20.) 10.
*)

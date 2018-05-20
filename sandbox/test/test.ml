open! Base
open! Stdio
open! Expect_test_helpers_kernel

let%expect_test _ =
  let l = [1;2;3;3;4;5;6;2;4;5;0;0;1] in
  let l = List.group ~break:(fun x y -> x > y) l in
  print_s [%sexp (l : int list list)];
  [%expect{|
    ((1 2 3 3 4 5 6)
     (2 4 5)
     (0 0 1)) |}]

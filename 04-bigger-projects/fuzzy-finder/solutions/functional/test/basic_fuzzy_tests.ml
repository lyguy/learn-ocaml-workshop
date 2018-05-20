open! Core
open! Expect_test_helpers_kernel
open! Functional_fzf_solution

let%expect_test _ =
  let open Fuzzy in
  let now = Time.epoch in
  let p m =
    let (w,_) = Model.widget_and_selected m ~now in
    printf "\n%s" (Tty_text.Widget.to_string w)
  in
  let m = Fuzzy.Model.create ~now in
  p m;
  [%expect{| |   > |}];
  let m = handle_line m "abc" in
  let m = handle_line m "ccc" in
  let m = handle_line m "aaabc" in
  p m;
  [%expect{|
    abc
    ccc
    aaabc
    |   > |}];
  let (m,_) = handle_user_input m (Char 'a') in
  p m;
  [%expect{|
    abc
    aaabc
    |   > a |}];
  let (m,_) = handle_user_input m (Char 'a') in
  p m;
  [%expect{|
    aaabc
    |   > aa |}];
  let (m,_) = handle_user_input m Backspace in
  p m;
  [%expect {|
    abc
    aaabc
    |   > a |}]

open! Core
open! Expect_test_helpers_kernel
open! Functional_fzf

let%expect_test _ =
  let open Fuzzy in
  let now = ref Time.epoch in
  let m = ref (Fuzzy.Model.create ~now:!now) in
  let update f = m := (f !m) in
  let set_now s =
    now := Time.add Time.epoch (Time.Span.of_sec s)
  in
  let p () =
    let (w,_) = Model.widget_and_selected !m ~now:!now in
    printf "\n%s" (Tty_text.Widget.to_string w)
  in
  p ();
  [%expect{| 0s 0 |}];
  update (fun m -> handle_line m "abc");
  update (fun m -> handle_line m "ccc");
  update (fun m -> handle_line m "aaabc");
  p ();
  [%expect{|
    0s 3 |}];
  update (fun m -> handle_line m "cccc");
  p ();
  [%expect{|
    0s 4 |}];
  set_now 2.;
  p ();
  [%expect{| 2s 4 |}]

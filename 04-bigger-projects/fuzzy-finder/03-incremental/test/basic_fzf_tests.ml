open! Core
open! Incremental_fzf
open! Expect_test_helpers_kernel

let%expect_test _ =
  let open Import in
  let open Incr.Let_syntax in
  let open Fuzzy in
  let now = Time.epoch in
  Incr.advance_clock ~to_:now;
  let m_v = Incr.Var.create (Model.create ~now) in
  let widget_and_selected = Model.widget_and_selected (Incr.Var.watch m_v) in
  let widget = widget_and_selected >>| fst in
  Incr.Observer.on_update_exn (Incr.observe widget)
    ~f:(function
        | Initialized w | Changed (_, w) ->
          printf "\n%s" (Tty_text.Widget.to_string w)
        | Invalidated -> ());
  let update f = Incr.Var.set m_v (f (Incr.Var.value m_v)) in
  Incr.stabilize ();
  [%expect{| |   > |}];
  update (fun m -> handle_line m "abc");
  update (fun m -> handle_line m "ccc");
  update (fun m -> handle_line m "aaabc");
  Incr.stabilize ();
  [%expect{|
    abc
    ccc
    aaabc
    |   > |}];
  update (fun m -> fst (handle_user_input m (Char 'a')));
  Incr.stabilize
  [%expect{| |}];
  update (fun m -> fst (handle_user_input m (Char 'a')));
  Incr.stabilize ();
  [%expect{|
    abc
    aaabc
    |   > a







































    aaabc
    |   > aa |}];
  update (fun m -> fst (handle_user_input m Backspace));
  Incr.stabilize ();
  [%expect {|
    abc
    aaabc
    |   > a |}]

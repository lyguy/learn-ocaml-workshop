open Core
open Async

module Dimensions = struct
  type t =
    { width  : int
    ; height : int
    }
end

module User_input = struct
  type t =
    | Ctrl_c
    | Escape
    | Backspace
    | Return (* Enter key *)
    | Char of char
    | Up
    | Down
  [@@deriving sexp_of]
end

module Configure_terminal = struct

  type t =
    { attr_in : Unix.Terminal_io.t
    ; attr_out: Unix.Terminal_io.t
    }

  let setattr_out fd ~attr_out =
    Unix.Terminal_io.tcsetattr
      attr_out
      ~mode:Unix.Terminal_io.TCSAFLUSH
      fd

  let setattr_in fd ~attr_in =
    Unix.Terminal_io.tcsetattr
      attr_in
      ~mode:Unix.Terminal_io.TCSADRAIN
      fd

  let get_current_settings ~input ~output () =
    let%bind attr_in  = Unix.Terminal_io.tcgetattr input in
    let%bind attr_out = Unix.Terminal_io.tcgetattr output in
    return { attr_in; attr_out }

  let set ~input ~output t =
    let%bind () = setattr_in  input  ~attr_in:t.attr_in   in
    let%bind () = setattr_out output ~attr_out:t.attr_out in
    return ()

  let map_termio (attrs : Core.Unix.Terminal_io.t) =
    { attrs with
      Core.Unix.Terminal_io.
      c_ignbrk = false;
      c_brkint = false;
      c_parmrk = false;
      c_istrip = false;
      c_inlcr  = false;
      c_igncr  = false;
      c_icrnl  = false;
      c_ixon   = false;
      c_opost  = false;
      c_echo   = false;
      c_echonl = false;
      c_icanon = false;
      c_isig   = false;
      c_csize  = 8;
      c_parenb = false;
      c_vmin = 1;
      c_vtime = 0;
    }
  ;;

  let to_drawing t =
    { attr_in  = map_termio t.attr_in
    ; attr_out = map_termio t.attr_out
    }
  ;;
end

let esc rest =
  "\x1b[" ^ rest
;;

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving enumerate]

  let escape = function
    | Up    -> esc "A"
    | Down  -> esc "B"
    | Right -> esc "C"
    | Left  -> esc "D"
end

module Action = struct
  type t =
    | Clear_screen
    | Move_cursor_to_home
    | Next_line
    | Move of Direction.t
    | Switch_to_alternate_buffer
    | Switch_from_alternate_buffer
    | Erase_to_end_of_line

  let _compilation_fix_for_unused_constructor = Move Left

  let to_string = function
    | Clear_screen -> esc "2J"
    | Erase_to_end_of_line -> esc "K"
    | Move_cursor_to_home -> esc "H"
    | Next_line -> "\r\n"
    | Move dir -> Direction.escape dir
    | Switch_to_alternate_buffer -> esc "?1049h"
    | Switch_from_alternate_buffer -> esc "?1049l"
end

let do_action writer action =
  Writer.write writer (Action.to_string action);
  Writer.flushed writer
;;

type t =
  { dimensions : Dimensions.t
  ; writer     : Writer.t
  }

let dimensions t =
  t.dimensions
;;

let stop_rendering t =
  do_action t Switch_from_alternate_buffer
;;

let with_rendering f =
  let%bind tty_reader = Reader.open_file ~buf_len:1 "/dev/tty" in
  let%bind tty_writer = Writer.open_file "/dev/tty" in
  let input  = Reader.fd tty_reader in
  let output = Writer.fd tty_writer in
  let%bind original = Configure_terminal.get_current_settings ~input ~output () in
  let restore () =
    Configure_terminal.set original ~input ~output
  in
  let%bind dimensions =
    let%map output =
      Process.run_exn ()
        ~prog:"stty"
        (* NOTE: for people on Mac OS X, use ~args:[ "-f"; "/dev/tty"; "size" ] *)
        ~args:[ "size"; "-F"; "/dev/tty" ]
    in
    match
      output
      |> String.strip
      |> String.split ~on:' '
    with
    | [height;width] ->
      { Dimensions.
        height = Int.of_string height
      ; width = Int.of_string width
      }
    | _ -> raise_s [%message "Could not determine terminal size"]
  in
  Monitor.protect ~finally:restore (fun () ->
    let t = { dimensions; writer = tty_writer } in
    let%bind () =
      Configure_terminal.to_drawing original
      |> Configure_terminal.set ~input ~output
    in
    let%bind () = do_action tty_writer Switch_to_alternate_buffer in
    let%bind () = do_action tty_writer Clear_screen in
    let user_input =
      Pipe.create_reader ~close_on_exception:true (fun w ->
        let repeat x =
          let%bind () = Pipe.write w x in
          return (`Repeat ())
        in
        let b = Bytes.create 1 in
        Deferred.repeat_until_finished () (fun () ->
          match%bind Reader.really_read ~len:1 tty_reader b with
          | `Eof _ -> return (`Finished ())
          | `Ok ->
            let char = Char.to_int (Bytes.get b 0) in
            Log.Global.sexp [%message "Character" (char : int)];
            match char with
            | 3 (* CTRL + C *) ->
              let%bind () = Pipe.write w User_input.Ctrl_c in
              return (`Finished ())
            | 0o177 -> repeat Backspace
            | 0o015 -> repeat Return
            | 0o033 ->
              (* beginning of an escape sequence *)
              begin
                let b = Bytes.create 1 in
                let next_read = Reader.really_read ~len:1 tty_reader b in
                match%bind
                  choose
                    [ choice (next_read :> [`Eof of int | `Ok | `Timed_out] Deferred.t) Fn.id
                    ; choice (Clock.after (sec 0.1)) (fun () -> `Timed_out) ]
                with
                | `Eof _ -> return (`Finished ())
                | `Timed_out ->
                  begin
                    let%bind () = Pipe.write w Escape in
                    match%bind next_read with
                    | `Eof _ -> return (`Finished ())
                    | `Ok -> return (`Finished ())
                  end
                | `Ok ->
                  let char = Bytes.get b 0 in
                  Log.Global.sexp [%message "Escape character" (char : char)];
                  match char with
                  | '[' ->
                    begin
                      let b = Bytes.create 1 in
                      match%bind Reader.really_read ~len:1 tty_reader b with
                      | `Eof _ -> return (`Finished ())
                      | `Ok ->
                        let char = Bytes.get b 0 in
                        Log.Global.sexp [%message "Escape sequence" (char : char)];
                        match char with
                        | 'A' -> repeat Up
                        | 'B' -> repeat Down
                        | _ ->
                          (* Skip unknown escape characters *)
                          return (`Repeat ())
                    end
                  | _ ->
                    (* Send the escape key, ignore the following
                       character if there is one. This is a bit of
                       hack, but it's OK in this app because we shut
                       down on escape *)
                    repeat Escape
              end
            | _     ->
              repeat (Char (Bytes.get b 0))
        )
      )
    in
    let%bind to_return = f (user_input, t) in
    let%bind () = restore () in
    let%bind () = stop_rendering t.writer in
    return to_return
  )
;;

module Widget = struct
  type t =
    | String of string
    | Hbox of t list
    | Vbox of t list

  let of_char c = String (String.of_char c)
  let of_string s = String s
  let hbox ts = Hbox ts
  let vbox ts = Vbox ts

  let to_string t =
    let module Block = Textutils.Text_block in
    let rec process = function
      | String x -> Block.text x
      | Hbox xs -> Block.hcat ~sep:(Block.text " ") (List.map ~f:process xs)
      | Vbox xs -> Block.vcat (List.map ~f:process xs)
    in
    Block.render (process t)

  let%expect_test _ =
    let p t = print_endline (to_string t) in
    p (Hbox
         [ Vbox [String "foo"; String "bar bar"]
         ; String "Whatever" ]);
    [%expect{|
      foo     Whatever
      bar bar |}]

  let render t writer =
    let string = to_string t in
    let lines = String.split_lines string in
    let act action = Writer.write writer (Action.to_string action) in
    act Move_cursor_to_home;
    List.iter lines ~f:(fun line ->
        Writer.write writer line;
        act Erase_to_end_of_line;
        act Next_line);
    act Erase_to_end_of_line;
    Writer.flushed writer
end

let render t w =
  Widget.render w t.writer
;;

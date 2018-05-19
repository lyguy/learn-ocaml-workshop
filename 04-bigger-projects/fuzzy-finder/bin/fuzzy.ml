open Core
open Async

(* TODO: Consider flipping direction to match ordinary fzf behavior *)
(* TODO: Fix selection: now the last filtered thing is shown, but
   really it should be the selection *)
(* TODO: Make incremental version *)
(* TODO: Show filter count *)

module Model = struct
  type t =
    { items: string Map.M(Int).t
    ; filter: string
    ; closed: bool
    ; dim: Screen_dimensions.t
    }

  let empty =
    { items = Map.empty (module Int)
    ; filter = ""
    ; closed = false
    ; dim = { width = 80; height = 40 }
    }

  let matches t =
    match t.filter with
    | "" -> t.items
    | _ ->
      let pattern = String.Search_pattern.create t.filter in
      Map.filter t.items ~f:(fun line ->
          Option.is_some (String.Search_pattern.index pattern ~in_:line))

  let view t ~start ~now =
    let open Tty_text in
    let matches = matches t in
    let matches_to_display =
      Map.to_sequence matches
      |> (fun matches -> Sequence.take matches (t.dim.height - 1))
      |> Sequence.map ~f:snd
      |> Sequence.map ~f:(fun s ->
          String.sub s ~pos:0 ~len:(Int.min (String.length s) t.dim.width))
      |> Sequence.to_list
    in
    let prompt = Widget.of_string ("> " ^ t.filter) in
    let extra_lines = t.dim.height - 1 - List.length matches_to_display in
    let spinner =
      if t.closed then []
      else
        [ Widget.of_string (String.of_char (Spinner.char ~spin_every:(sec 0.5) ~start ~now))
        ; Widget.of_string " "]
    in
    Widget.vbox
      (List.init extra_lines ~f:(fun _ -> Widget.of_string "")
       @ List.map matches_to_display ~f:Widget.of_string
       @ [ Widget.hbox (spinner @ [prompt])])
end

module Action = struct
  type t =
    | Exit
    | Exit_and_print
end

let handle_user_input (m:Model.t) (input:Tty_text.User_input.t) =
  match input with
  | Backspace ->
    let m = { m with filter = String.drop_suffix m.filter 1 } in
    (m,None)
  | Char x ->
    let m = { m with filter = m.filter ^ String.of_char x } in
    (m,None)
  | Return -> (m,Some Action.Exit_and_print)
  | Escape | Ctrl_c -> (m, Some Action.Exit)
  | Up -> (m, None)
  | Down -> (m, None)

let handle_line (m:Model.t) line =
  let lnum =
    match Map.max_elt m.items with
    | None -> 1
    | Some (num,_) -> num + 1
  in
  { m with items = Map.set m.items ~key:lnum ~data:line }

let handle_closed (m:Model.t) =
  { m with closed = true }

let run user_input tty_text ~start =
  let stdin = force Reader.stdin in
  let model_ref = ref Model.empty in
  let dirty = ref true in
  let finished = Ivar.create () in
  don't_wait_for (
    Pipe.iter_without_pushback (Reader.lines stdin) ~f:(fun line ->
        dirty := true;
        model_ref := handle_line !model_ref line));
  upon (Reader.close_finished stdin) (fun () ->
      dirty := true;
      model_ref := handle_closed !model_ref);
  don't_wait_for (
    Pipe.iter_without_pushback user_input ~f:(fun input ->
        dirty := true;
        let (model,action) = handle_user_input !model_ref input in
        model_ref := model;
        match action with
        | None -> ()
        | Some Exit ->
          Ivar.fill finished None
        | Some Exit_and_print ->
          let matches = Model.matches !model_ref in
          match Map.min_elt matches with
          | None ->
            Ivar.fill finished None
          | Some (_,line) ->
            Ivar.fill finished (Some line)));
  let finished = Ivar.read finished in
  Clock.every' (sec 0.1) ~stop:(Deferred.ignore finished)
    (fun () ->
       if not !dirty then Deferred.unit
       else (
         dirty := false;
         let dim = Tty_text.screen_dimensions tty_text in
         model_ref := { !model_ref with dim };
         Tty_text.render tty_text (Model.view !model_ref ~start ~now:(Time.now ()))));
  finished
;;

let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Custom fzf"
    (let%map_open () = return () in
     fun () ->
       Log.Global.set_output [Log.Output.file `Sexp ~filename:".fuzzy.log"];
       Log.Global.sexp [%message "Starting up"];
       let start = Time.now () in
       let open Deferred.Let_syntax in
       match%bind
         try_with (fun () ->
             Tty_text.with_rendering (fun (input, tty_text) ->
                 run input tty_text ~start))
       with
       | Error err ->
         print_s [%message "Failed with an exception" (err : exn)];
         Writer.flushed (force Writer.stdout)
       | Ok None ->
         return ()
       | Ok (Some output) ->
         print_endline output;
         Writer.flushed (force Writer.stdout))

let () = Command.run command

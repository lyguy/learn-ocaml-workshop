open Core
open Async

let run user_input tty_text ~start =
  let stdin = force Reader.stdin in
  let model_ref = ref Fuzzy.Model.empty in
  let dirty = ref true in
  let finished = Ivar.create () in
  don't_wait_for (
    Pipe.iter_without_pushback (Reader.lines stdin) ~f:(fun line ->
        dirty := true;
        model_ref := Fuzzy.handle_line !model_ref line));
  upon (Reader.close_finished stdin) (fun () ->
      dirty := true;
      model_ref := Fuzzy.handle_closed !model_ref (Time.now ()));
  don't_wait_for (
    Pipe.iter_without_pushback user_input ~f:(fun input ->
        dirty := true;
        let (model,action) = Fuzzy.handle_user_input !model_ref input in
        model_ref := model;
        match action with
        | None -> ()
        | Some Exit ->
          Ivar.fill finished None
        | Some Exit_and_print ->
          let matches = Fuzzy.Model.matches !model_ref in
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
         let dim = Tty_text.dimensions tty_text in
         model_ref := Fuzzy.set_dim !model_ref dim;
         Tty_text.render tty_text (Fuzzy.Model.to_widget !model_ref ~start ~now:(Time.now ()))));
  finished
;;

let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Simplified clone of fzf"
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

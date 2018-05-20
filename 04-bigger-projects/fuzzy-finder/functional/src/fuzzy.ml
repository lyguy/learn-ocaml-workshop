open! Base
open Import
module Widget = Tty_text.Widget

module Model = struct
  type t =
    { lines: string Map.M(Int).t
    ; start: Time.t
    }

  let create ~now =
    { lines = Map.empty (module Int)
    ; start = now
    }

  let widget_and_selected t ~now =
    let selected =
      match Map.min_elt t.lines with
      | None -> None
      | Some (_,line) -> Some line
    in
    let elapsed = Widget.of_string (Time.Span.to_string (Time.diff now t.start)) in
    let line = Widget.of_string (Int.to_string (Map.length t.lines)) in
    let widget = Widget.hbox [ elapsed; line ] in
    (widget, selected)
end

module Action = struct
  type t =
    | Exit
    | Exit_and_print
end

let handle_user_input (m:Model.t) _input =
  (m,None)

let handle_line (m:Model.t) line : Model.t =
  let lnum =
    match Map.max_elt m.lines with
    | None -> 1
    | Some (num,_) -> num + 1
  in
  { m with lines = Map.set m.lines ~key:lnum ~data:line }

let handle_closed (m:Model.t) _ =
  m

let set_dim (m:Model.t) _ =
  m

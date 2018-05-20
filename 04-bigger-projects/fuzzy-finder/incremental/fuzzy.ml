open! Base
open! Import
open Incr.Let_syntax

module Model = struct
  type t =
    { items: string Map.M(Int).t
    ; filter: string
    ; closed_at: Time.t option
    ; dim: Tty_text.Dimensions.t
    ; start: Time.t
    } [@@deriving fields]

  let create ~now =
    { items = Map.empty (module Int)
    ; filter = ""
    ; closed_at = None
    ; dim = { width = 80; height = 40 }
    ; start = now
    }

  let matches ~filter ~items =
    let%bind filter = filter in
    let re = Re.compile (Re.str filter) in
    Incr_map.filter_mapi items ~f:(fun ~key:_ ~data:line ->
        if Re.execp re line then Some line else None)

  let spin ~start ~spin_every ~now =
    let elapsed = Time.diff now start in
    let phase =
      Time.Span.(elapsed // spin_every)
      |> Int.of_float
      |> (fun x -> x % 4)
    in
    match phase with
    | 0 -> "|"
    | 1 -> "/"
    | 2 -> "-"
    | 3 -> "\\"
    | _ -> assert false

  let widget_and_selected t =
    let open Incr.Let_syntax in
    let module Widget = Tty_text.Widget in
    let dim = t >>| dim in
    let filter = t >>| filter in
    let items = t >>| items in
    let matches_to_display =
      let%map matches = matches ~filter ~items and dim = dim in
      Map.to_sequence matches
      |> (fun matches -> Sequence.take matches (dim.height - 1))
      |> Sequence.map ~f:snd
      |> Sequence.to_list
    in
    let spinner =
      let%map t = t and now = Incr.watch_now () in
      match t.closed_at with
      | Some closed_at ->
        [ Widget.of_string (Time.Span.to_string (Time.diff closed_at t.start))
        ; Widget.of_string " " ]
      | None ->
        [ Widget.of_string (spin ~spin_every:(Time.Span.of_sec 0.5) ~start:t.start ~now)
        ; Widget.of_string " " ]
    in
    let%map matches_to_display = matches_to_display
    and spinner = spinner
    and filter = filter
    and dim = dim
    in
    let prompt = Widget.of_string ("> " ^ filter) in
    let extra_lines = dim.height - 1 - List.length matches_to_display in
    let selected = List.hd matches_to_display in
    let matches_to_display =
      List.map matches_to_display ~f:(fun s ->
          String.sub s ~pos:0 ~len:(min dim.width (String.length s)))
    in
    let widget =
      Widget.vbox
        (List.init extra_lines ~f:(fun _ -> Widget.of_string "")
         @ List.map matches_to_display ~f:Widget.of_string
         @ [ Widget.hbox (spinner @ [prompt])])
    in
    (widget, selected)
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

let handle_closed (m:Model.t) time =
  { m with closed_at = Some time }

let set_dim (m:Model.t) dim =
  { m with dim }

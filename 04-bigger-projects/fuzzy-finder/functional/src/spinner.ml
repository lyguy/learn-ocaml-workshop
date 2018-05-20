open Core

let char ~start ~spin_every ~now =
  let elapsed = Time.diff now start in
  let phase =
    Time.Span.(elapsed // spin_every)
    |> Int.of_float
    |> (fun x -> x % 4)
  in
  match phase with
  | 0 -> '|'
  | 1 -> '/'
  | 2 -> '-'
  | 3 -> '\\'
  | _ -> assert false

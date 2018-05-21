open! Core
open! Async

let run _filter =
  let pipe = Reader.lines (force Reader.stdin) in
  let handle_line line =
    print_endline line;
    Writer.flushed (force Writer.stdout)
  in
  Pipe.iter pipe ~f:handle_line

let run _filter =
  Pipe.iter (Reader.lines (force Reader.stdin)) ~f:(fun line ->
      (* EXERCISE: Filter down to the matching lines.

         You can use Re for the pattern matching. Check out the functions:

         - Re.str
         - Re.compile
         - Re.execp
      *)
      print_endline line;
      Writer.flushed (force Writer.stdout))

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Read stdin, spit out lines that contain a match of the string in question"
    (let%map_open filter = anon ("filter" %: string) in
     fun () -> run filter)

let () = Command.run command

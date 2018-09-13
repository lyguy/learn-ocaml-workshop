(* Run [dune runtest] to build this file and run test it contains

   Goal: fix the compilation error, and make the test pass.
*)
open Base
open Stdio

let () = printf "Hello, World!

let%test "1+1 is 2" = 1 + 1 = 3

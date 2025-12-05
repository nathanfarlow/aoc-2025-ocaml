open! Core
open! Common

let part1 _ = failwith ""
let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let _ = return () in
  many_lines_of (many _) |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

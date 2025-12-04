open! Core
open! Common

let find_accessible grid =
  Grid.find_all grid ~f:(fun (i, j) cell ->
    let num_neighbors = Grid.neighbors8 grid (i, j) |> List.count ~f:snd in
    cell && num_neighbors < 4)
;;

let part1 (grid : bool Grid.t) = find_accessible grid |> List.length |> print_int
let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let cell = char '.' *> return false <|> char '@' *> return true in
  many_lines_of (many cell) |> exec_exn >> Grid.create
;;

let () = run_with_input_file ~part1 ~part2 ~parse

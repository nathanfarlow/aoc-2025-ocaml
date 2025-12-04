open! Core
open! Common

let is_accessible grid pos cell =
  let num_neighbors = Grid.neighbors8 grid pos |> List.count ~f:snd in
  cell && num_neighbors < 4
;;

let part1 grid = Grid.count grid ~f:(is_accessible grid) |> print_int

let part2 grid =
  let count = Grid.count ~f:(fun _ c -> c) in
  let initial_count = count grid in
  let rec delete_accessible () =
    Grid.find_opt grid ~f:(is_accessible grid)
    |> Option.iter ~f:(fun (i, j) ->
      grid.(i).(j) <- false;
      delete_accessible ())
  in
  delete_accessible ();
  print_int (initial_count - count grid)
;;

let parse =
  let open Angstrom in
  let cell = char '.' *> return false <|> char '@' *> return true in
  many_lines_of (many cell) |> exec_exn >> Grid.create
;;

let () = run_with_input_file ~part1 ~part2 ~parse

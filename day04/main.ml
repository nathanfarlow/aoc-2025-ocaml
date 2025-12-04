open! Core
open! Common

let find_accessible grid =
  Grid.find_all grid ~f:(fun (i, j) cell ->
    let num_neighbors = Grid.neighbors8 grid (i, j) |> List.count ~f:snd in
    cell && num_neighbors < 4)
;;

let part1 = find_accessible >> List.length >> print_int

let part2 grid =
  let count = Grid.find_all ~f:(fun _ c -> c) >> List.length in
  let rec delete_some () =
    let old = Grid.copy grid in
    find_accessible grid |> List.iter ~f:(fun (i, j) -> grid.(i).(j) <- false);
    if not @@ Grid.equal Bool.equal old grid then delete_some ()
  in
  let initial_count = count grid in
  delete_some ();
  print_int (initial_count - count grid)
;;

let parse =
  let open Angstrom in
  let cell = char '.' *> return false <|> char '@' *> return true in
  many_lines_of (many cell) |> exec_exn >> Grid.create
;;

let () = run_with_input_file ~part1 ~part2 ~parse

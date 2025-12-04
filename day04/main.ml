open! Core
open! Common

let part1 (grid : bool Grid.t) =
  List.range 0 (Grid.height grid)
  |> List.cartesian_product (List.range 0 (Grid.width grid))
  |> sum ~f:(fun (i, j) ->
    let neighbors = Grid.neighbors8 grid (i, j) in
    let num_neighbors = List.count neighbors ~f:snd in
    if Grid.get grid (i, j) && num_neighbors < 4 then 1 else 0)
  |> print_int
;;

let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let cell = char '.' *> return false <|> char '@' *> return true in
  many_lines_of (many cell) |> exec_exn >> Grid.create
;;

let () = run_with_input_file ~part1 ~part2 ~parse

open! Core
open! Common

type direction =
  | LEFT
  | RIGHT

type move =
  { direction : direction
  ; amount : int
  }

let part1 puzzle =
  List.fold puzzle ~init:(50, 0) ~f:(fun (position, num_zeros) move ->
    let position =
      let num_positions = 100 in
      match move.direction with
      | LEFT -> (position + (num_positions - move.amount)) % num_positions
      | RIGHT -> (position + move.amount) % num_positions
    in
    let num_zeros = if position = 0 then num_zeros + 1 else num_zeros in
    position, num_zeros)
  |> snd
  |> print_int
;;

let part2 puzzle =
  List.concat_map puzzle ~f:(fun move ->
    List.init move.amount ~f:(fun _ -> { move with amount = 1 }))
  |> part1
;;

let parse =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let move =
    let%bind direction = char 'L' *> return LEFT <|> char 'R' *> return RIGHT in
    let%map amount = integer in
    { direction; amount }
  in
  many_lines_of move |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

open! Core
open! Common

let num_positions = 100

let part1 puzzle =
  List.fold puzzle ~init:(50, 0) ~f:(fun (position, num_zeros) amount ->
    let position = (position + amount) % num_positions in
    let num_zeros = if position = 0 then num_zeros + 1 else num_zeros in
    position, num_zeros)
  |> snd
  |> print_int
;;

let part2 puzzle =
  List.concat_map puzzle ~f:(fun amount ->
    List.init (abs amount) ~f:(fun _ -> amount / abs amount))
  |> part1
;;

let parse =
  let open Angstrom in
  let amount =
    let%bind direction = char 'L' *> return Int.neg <|> char 'R' *> return Fn.id in
    integer >>| direction
  in
  many_lines_of amount |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

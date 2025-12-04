open! Core
open! Common

let part1 =
  List.fold ~init:(50, 0) ~f:(fun (position, num_zeros) amount ->
    let position = (position + amount) % 100 in
    position, if position = 0 then num_zeros + 1 else num_zeros)
  >> snd
  >> print_int
;;

let part2 =
  List.concat_map ~f:(fun amount ->
    List.init (abs amount) ~f:(fun _ -> amount / abs amount))
  >> part1
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

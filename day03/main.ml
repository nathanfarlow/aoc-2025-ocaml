open! Core
open! Common

let rec largest_joltage num_digits all_digits =
  match num_digits with
  | 0 -> 0
  | _ ->
    let max_digit =
      List.take all_digits (List.length all_digits - num_digits + 1)
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let remaining = List.drop_while all_digits ~f:(( <> ) max_digit) |> List.tl_exn in
    (Int.pow 10 (num_digits - 1) * max_digit) + largest_joltage (num_digits - 1) remaining
;;

let part k = sum ~f:(largest_joltage k) >> print_int

let parse =
  let open Angstrom in
  let digit = satisfy Char.is_digit >>| Char.to_string >>| Int.of_string in
  many_lines_of (many digit) |> exec_exn
;;

let () = run_with_input_file ~part1:(part 2) ~part2:(part 12) ~parse

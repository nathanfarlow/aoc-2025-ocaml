open! Core
open! Common

let rec choose k xs =
  match k with
  | 0 -> 0
  | _ ->
    let max =
      List.take xs (List.length xs - k + 1)
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let remaining = List.drop_while xs ~f:(( <> ) max) |> List.tl_exn in
    (Int.pow 10 (k - 1) * max) + choose (k - 1) remaining
;;

let part k = sum ~f:(choose k) >> print_int

let parse =
  let open Angstrom in
  let digit = satisfy Char.is_digit >>| Char.to_string >>| Int.of_string in
  many_lines_of (many digit) |> exec_exn
;;

let () = run_with_input_file ~part1:(part 2) ~part2:(part 12) ~parse

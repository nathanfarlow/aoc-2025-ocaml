open! Core
open! Common

let would_easily_fit ((w, h), elems) =
  let blocks_that_can_fit = w / 3 * (h / 3) in
  let num_blocks = sum elems ~f:Fn.id in
  num_blocks <= blocks_that_can_fit
;;

let part1 = List.count ~f:would_easily_fit >> print_int
let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let line =
    let%bind dims = lift2 Tuple2.create (integer <* char 'x') integer in
    let%bind _ = string ": " in
    let%map rest = sep_by1 (char ' ') integer in
    dims, rest
  in
  let lines =
    let%bind first = skip_till_p line <* char '\n' in
    let%map rest = many_lines_of line in
    first :: rest
  in
  exec_exn lines
;;

let () = run_with_input_file ~part1 ~part2 ~parse

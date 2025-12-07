open! Core
open! Common

let part1 (ranges, available_ids) =
  List.count available_ids ~f:(fun id ->
    List.exists ranges ~f:(fun (start, end_) -> start <= id && id <= end_))
  |> print_int
;;

let part2 (ranges, _) =
  List.sort ranges ~compare:[%compare: int * int]
  |> (function
   | [] -> []
   | hd :: tl ->
     List.fold tl ~init:(hd, []) ~f:(fun ((c_start, c_end), all) (r_start, r_end) ->
       if c_start <= r_start && r_start <= c_end
       then (c_start, max c_end r_end), all
       else (r_start, r_end), (c_start, c_end) :: all)
     |> Tuple2.uncurry List.cons)
  |> sum ~f:(fun (a, b) -> b - a + 1)
  |> print_int
;;

let parse =
  let open Angstrom in
  let ranges =
    let range = both (integer <* char '-') integer in
    many_lines_of range
  in
  let available_ids = many_lines_of integer in
  both (ranges <* space_or_line) available_ids |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

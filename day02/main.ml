open! Core
open! Common

let is_invalid s size =
  String.to_list s
  |> List.chunks_of ~length:size
  |> List.all_equal ~equal:[%equal: char list]
  |> Option.is_some
;;

let print_sum = sum ~f:Int.of_string >> print_int

let part1 =
  List.filter ~f:(fun i -> is_invalid i (String.length i // 2 |> Float.iround_up_exn))
  >> print_sum
;;

let part2 =
  List.filter ~f:(fun i ->
    List.range 1 ((String.length i / 2) + 1) |> List.exists ~f:(is_invalid i))
  >> print_sum
;;

let parse s =
  let open Angstrom in
  let range = both (integer <* char '-') integer in
  let ranges = exec_exn (sep_by (char ',') range) s in
  List.concat_map ranges ~f:(fun (start, end_) ->
    List.range start (end_ + 1) |> List.map ~f:Int.to_string)
;;

let () = run_with_input_file ~part1 ~part2 ~parse

open! Core
open! Common
open Angstrom

let part1 (rows, ops) =
  List.transpose_exn rows
  |> List.zip_exn ops
  |> sum ~f:(fun (f, ints) -> List.reduce_exn ints ~f)
  |> print_int
;;

let op = char '*' *> return ( * ) <|> char '+' *> return ( + )

let parse1 =
  let ints = sep_by1 space integer in
  both (many_lines_of ints) (sep_by space op) |> exec_exn
;;

let rec split nums xs =
  match nums with
  | [] -> []
  | n :: rest ->
    let fst, snd = List.split_n xs n in
    fst :: split rest snd
;;

let part2 (rows, (ops, nums)) =
  List.map rows ~f:(split nums)
  |> List.transpose_exn
  |> List.map ~f:List.transpose_exn
  |> List.map ~f:(fun column ->
    List.filter_map column ~f:(String.concat >> Int.of_string_opt))
  |> List.zip_exn ops
  |> sum ~f:(fun (f, ints) -> List.reduce_exn ints ~f)
  |> print_int
;;

let parse2 =
  let row = many1 Char.(satisfy is_digit <|> char ' ' >>| to_string >>| String.strip) in
  let ops = both op (many (char ' ') >>| List.length >>| ( + ) 1) in
  both (many_lines_of ~trim:false row) (many ops >>| List.unzip)
  |> exec_exn ~trim:false ~consume:Prefix
;;

let () =
  run_with_input_file ~part1:(parse1 >> part1) ~part2:(parse2 >> part2) ~parse:Fn.id
;;

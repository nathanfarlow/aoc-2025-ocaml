open! Core
open! Common
open Angstrom

let zip_and_print ops =
  List.zip_exn ops >> sum ~f:(fun (f, ints) -> List.reduce_exn ints ~f) >> print_int
;;

let part1 (rows, ops) = List.transpose_exn rows |> zip_and_print ops
let op = char '*' *> return ( * ) <|> char '+' *> return ( + )

let parse1 =
  let ints = sep_by1 space integer in
  both (many_lines_of ints) (sep_by space op) |> exec_exn
;;

let rec split sizes xs =
  match sizes with
  | [] -> []
  | size :: sizes ->
    let fst, snd = List.split_n xs size in
    fst :: split sizes snd
;;

let part2 (rows, (ops, sizes)) =
  List.map rows ~f:(split sizes)
  |> List.transpose_exn
  |> List.map ~f:List.transpose_exn
  |> List.map ~f:(List.filter_map ~f:(String.concat >> Int.of_string_opt))
  |> zip_and_print ops
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

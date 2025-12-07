open! Core
open! Common

let op =
  let open Angstrom in
  char '*' *> return ( * ) <|> char '+' *> return ( + )
;;

let parse_part1 =
  let open Angstrom in
  let ints = sep_by1 space integer <* space in
  both (many_lines_of ints) (sep_by space op) |> exec_exn
;;

let part1 (rows, ops) =
  List.transpose_exn rows
  |> List.zip_exn ops
  |> sum ~f:(fun (f, ints) -> List.reduce_exn ints ~f)
  |> print_int
;;

let part1 = parse_part1 >> part1

let parse_part2 =
  let open Angstrom in
  let int_line =
    many1 (satisfy Char.is_digit <|> char ' ' >>| Char.to_string >>| String.strip)
  in
  let ops =
    let%bind op = char '*' *> return `Mul <|> char '+' *> return `Add in
    let%bind count = many (char ' ') >>| List.length in
    return (op, count + 1)
  in
  let p =
    let%bind ints = many_lines_of ~trim:false int_line in
    let%bind ops = many ops <* space_or_line in
    return (ints, ops)
  in
  p |> exec_exn ~trim:false
;;

let rec split nums xs =
  match nums with
  | [] -> []
  | n :: rest ->
    let fst, snd = List.split_n xs n in
    fst :: split rest snd
;;

let part2 ((p : string list list), (k : ([> `Add | `Mul ] * int) list)) =
  let _ops = List.map k ~f:fst in
  let nums = List.map k ~f:snd in
  let rows = List.map p ~f:(split nums) in
  let _cols = List.transpose_exn rows in
  let _hm = List.map _cols ~f:List.transpose_exn in
  let _int =
    List.map _hm ~f:(fun column ->
      List.filter_map column ~f:(fun node -> String.concat node |> Int.of_string_opt))
  in
  List.zip_exn _ops _int
  |> sum ~f:(fun (op, ints) ->
    match op with
    | `Mul -> List.reduce_exn ints ~f:( * )
    | `Add -> List.reduce_exn ints ~f:( + ))
  |> print_int
;;

let part2 = parse_part2 >> part2
let () = run_with_input_file ~part1 ~part2 ~parse:Fn.id

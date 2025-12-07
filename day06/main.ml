open! Core
open! Common

let parse_part1 =
  let open Angstrom in
  let ints = sep_by1 space integer <* space in
  let op = char '*' *> return `Mul <|> char '+' *> return `Add in
  both (many_lines_of ints) (sep_by space op) |> exec_exn
;;

let part1 ((ints : int list list), (ops : [ `Mul | `Add ] list)) =
  let todo = List.transpose_exn ints |> List.zip_exn ops in
  sum todo ~f:(fun (op, ints) ->
    match op with
    | `Mul -> List.reduce_exn ints ~f:( * )
    | `Add -> List.reduce_exn ints ~f:( + ))
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
    (* print_s [%message (ints : string list list)]; *)
    (* print_s [%message (ops : ([ `Add | `Mul ] * int) list)]; *)
    return (ints, ops)
  in
  p |> exec_exn ~trim:false
;;

let rec split xs nums =
  match nums with
  | [] -> []
  | n :: rest ->
    let fst, snd = List.split_n xs n in
    fst :: split snd rest
;;

let part2 ((p : string list list), (k : ([> `Add | `Mul ] * int) list)) =
  let _ops = List.map k ~f:fst in
  print_s [%message (p : string list list)];
  print_s [%message (_ops : [ `Add | `Mul ] list)];
  let nums = List.map k ~f:snd in
  print_s [%message (nums : int list)];
  let rows = List.map p ~f:(fun row -> split row nums) in
  print_s [%message (rows : string list list list)];
  let _cols = List.transpose_exn rows in
  let _hm = List.map _cols ~f:List.transpose_exn in
  print_s [%message (_cols : string list list list)];
  print_s [%message (_hm : string list list list)];
  let _int =
    List.map _hm ~f:(fun column ->
      List.filter_map column ~f:(fun node -> String.concat node |> Int.of_string_opt))
  in
  print_s [%message (_int : int list list)];
  List.zip_exn _ops _int
  |> sum ~f:(fun (op, ints) ->
    print_s [%message (op : [ `Mul | `Add ]) (ints : int list)];
    match op with
    | `Mul -> List.reduce_exn ints ~f:( * )
    | `Add -> List.reduce_exn ints ~f:( + ))
  |> print_int
;;

let part2 = parse_part2 >> part2
let () = run_with_input_file ~part1 ~part2 ~parse:Fn.id

open! Core
module Math = Math
module Grid = Grid
module Point = Point

module Syntax = struct
  include Option.Let_syntax

  let ( >> ) f g x = g (f x)
end

include Syntax

module Angstrom = struct
  include Angstrom
  include Angstrom.Let_syntax

  let space =
    skip_while (function
      | ' ' | '\t' -> true
      | _ -> false)
  ;;

  let integer =
    let%bind mul = option 1 (char '-' >>| fun _ -> -1) in
    take_while1 Char.is_digit >>| Int.of_string >>| ( * ) mul
  ;;

  let ws = skip_while Char.is_whitespace
  let many_lines_of p = sep_by end_of_line p

  let exec_exn ?(trim = true) ?(consume = Angstrom.Consume.All) parser s =
    let s = if trim then String.strip s else s in
    parse_string ~consume parser s |> Result.ok_or_failwith
  ;;

  let exec_opt ?trim ?consume parser s =
    match exec_exn ?trim ?consume parser s with
    | x -> Some x
    | exception _ -> None
  ;;

  let skip_till p = fix (fun m -> p <|> any_char *> m)
  let digit = satisfy Char.is_digit >>| Char.to_string >>| Int.of_string
end

let zip_next l =
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | x :: (y :: _ as xs) -> aux ((x, y) :: acc) xs
  in
  aux [] l |> List.rev
;;

let%expect_test "zip_next" =
  zip_next [ 1; 2; 3; 4 ] |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| ((1 2) (2 3) (3 4)) |}];
  zip_next [ 1; 2 ] |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| ((1 2)) |}];
  zip_next [ 1 ] |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| () |}];
  zip_next [] |> [%sexp_of: (int * int) list] |> print_s;
  [%expect {| () |}]
;;

let sum ~f = List.sum (module Int) ~f
let print_int = printf "%d\n"

let triangular l =
  let open Sequence.Generator in
  let rec aux = function
    | [] -> return ()
    | x :: xs -> yield (x, xs) >>= fun () -> aux xs
  in
  run (aux l)
;;

let%expect_test "triangular" =
  let print = [%sexp_of: (string * string list) Sequence.t] >> print_s in
  triangular [ "a"; "b"; "c" ] |> print;
  [%expect {| ((a (b c)) (b (c)) (c ())) |}];
  triangular [ "a"; "b" ] |> print;
  [%expect {| ((a (b)) (b ())) |}];
  triangular [ "a" ] |> print;
  [%expect {| ((a ())) |}];
  triangular [] |> print;
  [%expect {| () |}]
;;

let all_pairs l =
  Sequence.concat_map (triangular l) ~f:(fun (x, xs) ->
    Sequence.of_list (List.map xs ~f:(fun y -> x, y)))
;;

let%expect_test "all_pairs" =
  let print = [%sexp_of: (string * string) Sequence.t] >> print_s in
  all_pairs [ "a"; "b"; "c" ] |> print;
  [%expect {| ((a b) (a c) (b c)) |}];
  all_pairs [ "a"; "b" ] |> print;
  [%expect {| ((a b)) |}];
  all_pairs [ "a" ] |> print;
  [%expect {| () |}];
  all_pairs [] |> print;
  [%expect {| () |}]
;;

let run_with_input_file ~part1 ~part2 ~parse =
  Command.basic
    ~summary:"Advent of code"
    (let%map_open.Command f =
       choose_one
         ~if_nothing_chosen:
           (Default_to
              (fun s ->
                print_endline "Part 1:";
                part1 s;
                print_endline "Part 2:";
                part2 s))
         [ flag "--part1" no_arg ~doc:"Run part 1"
           |> map ~f:(fun b -> Option.some_if b part1)
         ; flag "--part2" no_arg ~doc:"Run part 2"
           |> map ~f:(fun b -> Option.some_if b part2)
         ]
     and input = anon ("input file" %: string) in
     fun () -> In_channel.read_all input |> parse |> f)
  |> Command_unix.run
;;

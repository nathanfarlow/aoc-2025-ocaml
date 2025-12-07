open! Core
open! Common

[@@@warning "-37"]

type cell =
  | Empty
  | Start
  | Split
  | Beam

let calculate grid =
  Memo.recursive ~hashable:Point.hashable (fun calculate (i, j) ->
    let c = ref 0 in
    let rec loop i =
      match Grid.get_opt grid (i, j) with
      | Some Start -> incr c
      | None | Some Split -> ()
      | _ ->
        [ i, j - 1; i, j + 1 ]
        |> List.iter ~f:(fun pos ->
          match Grid.get_opt grid pos with
          | Some Split -> c := !c + calculate pos
          | _ -> ());
        loop (i - 1)
    in
    loop (i - 1);
    !c)
;;

let part2 grid =
  let calculate = calculate grid in
  Grid.find_all grid ~f:(fun _ -> function
    | Split -> true
    | _ -> false)
  |> sum ~f:calculate
  |> ( + ) 1
  |> print_int
;;

let part1 grid =
  let calculate = calculate grid in
  Grid.find_all grid ~f:(fun _ -> function
    | Split -> true
    | _ -> false)
  |> List.map ~f:calculate
  |> List.count ~f:(( < ) 0)
  |> print_int
;;

let parse =
  let open Angstrom in
  let line =
    let one =
      char '.' *> return Empty <|> char 'S' *> return Start <|> char '^' *> return Split
    in
    many one
  in
  many_lines_of line >>| Grid.create |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

open! Core
open! Common

let num_visits grid =
  Memo.recursive ~hashable:Point.hashable (fun num_visits (i, j) ->
    let rec loop i total =
      match Grid.get_opt grid (i, j) with
      | None | Some '^' -> total
      | Some 'S' -> total + 1
      | _ ->
        let total =
          total
          + ([ i, j - 1; i, j + 1 ]
             |> sum ~f:(fun pos ->
               match Grid.get_opt grid pos with
               | Some '^' -> num_visits pos
               | _ -> 0))
        in
        loop (i - 1) total
    in
    loop (i - 1) 0)
;;

let part1 grid =
  Grid.find_all grid ~f:(fun _ -> Char.equal '^')
  |> List.map ~f:(num_visits grid)
  |> List.count ~f:(( < ) 0)
  |> print_int
;;

let part2 grid =
  Grid.find_all grid ~f:(fun _ -> Char.equal '^')
  |> sum ~f:(num_visits grid)
  |> ( + ) 1
  |> print_int
;;

let parse =
  let open Angstrom in
  many_lines_of (many (not_char '\n')) >>| Grid.create |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

open! Core
open! Common

let calculate grid =
  Memo.recursive ~hashable:Point.hashable (fun calculate (i, j) ->
    let c = ref 0 in
    let rec loop i =
      match Grid.get_opt grid (i, j) with
      | Some 'S' -> incr c
      | None | Some '^' -> ()
      | _ ->
        [ i, j - 1; i, j + 1 ]
        |> List.iter ~f:(fun pos ->
          match Grid.get_opt grid pos with
          | Some '^' -> c := !c + calculate pos
          | _ -> ());
        loop (i - 1)
    in
    loop (i - 1);
    !c)
;;

let part2 grid =
  Grid.find_all grid ~f:(fun _ -> Char.equal '^')
  |> sum ~f:(calculate grid)
  |> ( + ) 1
  |> print_int
;;

let part1 grid =
  Grid.find_all grid ~f:(fun _ -> Char.equal '^')
  |> List.map ~f:(calculate grid)
  |> List.count ~f:(( < ) 0)
  |> print_int
;;

let parse =
  let open Angstrom in
  many_lines_of (many (not_char '\n')) >>| Grid.create |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

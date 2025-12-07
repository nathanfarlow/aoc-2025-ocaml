open! Core
open! Common

type cell =
  | Empty
  | Start
  | Split
  | Beam

let step grid =
  Array.iteri grid ~f:(fun i ->
    Array.iteri ~f:(fun j cell ->
      match cell with
      | Beam ->
        let below = i + 1, j in
        (match Grid.get_opt grid below with
         | Some Split ->
           Grid.set_opt grid (i + 1, j + 1) Beam;
           Grid.set_opt grid (i + 1, j - 1) Beam
         | Some _ -> Grid.set grid below Beam
         | None -> ())
      | _ -> ()))
;;

let part1 grid =
  let s_i, s_j =
    Grid.find_exn grid ~f:(fun _ -> function
      | Start -> true
      | _ -> false)
  in
  Grid.set grid (s_i + 1, s_j) Beam;
  for _ = 1 to Grid.height grid do
    step grid
  done;
  let num_splits =
    Grid.find_all grid ~f:(fun _ -> function
      | Split -> true
      | _ -> false)
    |> List.count ~f:(fun (i, j) ->
      match Grid.get_opt grid (i - 1, j) with
      | Some Beam -> true
      | _ -> false)
  in
  print_int num_splits
;;

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

open! Core
open! Common

let area ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let part1 coords =
  all_pairs coords
  |> Sequence.map ~f:area
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.iter ~f:print_int
;;

let w = 100_000

let part2 coords =
  let grid = Bigarray.Array2.create Bigarray.int8_unsigned Bigarray.c_layout w w in
  Bigarray.Array2.fill grid 0;
  let set (i, j) v = grid.{i, j} <- Bool.to_int v in
  let get (i, j) = grid.{i, j} <> 0 in
  let in_bounds (i, j) = 0 <= i && i < w && 0 <= j && j < w in
  let iter_rect ((x1, y1), (x2, y2)) ~f =
    for y = min y1 y2 to max y1 y2 do
      for x = min x1 x2 to max x1 x2 do
        f (y, x)
      done
    done
  in
  let coords = coords @ [ List.hd_exn coords ] in
  zip_next coords |> List.iter ~f:(iter_rect ~f:(fun coord -> set coord true));
  let queue = Queue.create () in
  Queue.enqueue queue (0, 0);
  let rec mark () =
    match Queue.dequeue queue with
    | None -> ()
    | Some coord ->
      set coord true;
      List.iter (Point.neighbors4 coord) ~f:(fun neighbor ->
        if in_bounds neighbor && not (get neighbor)
        then (
          set neighbor true;
          Queue.enqueue queue neighbor));
      mark ()
  in
  mark ();
  zip_next coords |> List.iter ~f:(iter_rect ~f:(fun coord -> set coord false));
  all_pairs coords
  |> Sequence.to_list
  |> List.sort ~compare:(Comparable.lift (Comparable.reverse Int.compare) ~f:area)
  |> List.find ~f:(fun pair ->
    with_return (fun r ->
      iter_rect pair ~f:(fun (y, x) -> if get (y, x) then r.return false);
      true))
  |> Option.map ~f:area
  |> Option.iter ~f:print_int
;;

let parse =
  let open Angstrom in
  many_lines_of (both (integer <* char ',') integer) |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

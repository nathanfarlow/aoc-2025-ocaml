open! Core
open! Common

module Point3 = struct
  type t = int * int * int [@@deriving compare, hash, equal, sexp_of]

  let distance ((a, b, c), (x, y, z)) = sum [ x - a; y - b; z - c ] ~f:(fun x -> x * x)
end

module Graph = struct
  let create () = Hashtbl.create (module Point3)

  let all_in_group t a =
    let visited = Hash_set.create (module Point3) in
    let rec dfs coord =
      if not (Hash_set.mem visited coord)
      then (
        Hash_set.add visited coord;
        Hashtbl.find t coord |> Option.iter ~f:(List.iter ~f:dfs))
    in
    dfs a;
    Hash_set.to_list visited |> List.sort ~compare:Point3.compare
  ;;

  let connect t a b =
    Hashtbl.add_multi t ~key:a ~data:b;
    Hashtbl.add_multi t ~key:b ~data:a
  ;;

  let is_directly_connected t (a, b) =
    match Hashtbl.find t a with
    | None -> false
    | Some b' -> List.mem ~equal:Point3.equal b' b
  ;;
end

let connect coords ~until =
  let all_pairs = all_pairs coords |> Sequence.to_list in
  let adj = Graph.create () in
  let rec loop () =
    let a, b =
      all_pairs
      |> List.filter ~f:(Graph.is_directly_connected adj >> not)
      |> List.min_elt ~compare:(Comparable.lift Int.compare ~f:Point3.distance)
      |> Option.value_exn
    in
    Graph.connect adj a b;
    if until adj (a, b) then a, b else loop ()
  in
  adj, loop ()
;;

let part1 coords =
  let count = ref 0 in
  let adj, _ =
    connect coords ~until:(fun _ _ ->
      incr count;
      !count = 1000)
  in
  List.map coords ~f:(Graph.all_in_group adj)
  |> List.dedup_and_sort ~compare:[%compare: Point3.t list]
  |> List.map ~f:List.length
  |> List.sort ~compare:(Comparable.reverse Int.compare)
  |> (fun l -> List.take l 3)
  |> List.reduce_exn ~f:( * )
  |> print_int
;;

let part2 coords =
  let _, ((ax, _, _), (bx, _, _)) =
    connect coords ~until:(fun adj (a, _) ->
      Graph.all_in_group adj a |> List.length = List.length coords)
  in
  print_int (ax * bx)
;;

let parse =
  let open Angstrom in
  let coord = lift3 Tuple3.create (integer <* char ',') (integer <* char ',') integer in
  many_lines_of coord |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

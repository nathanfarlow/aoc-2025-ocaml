open! Core
open! Common

let sq x = x * x

module Point3 = struct
  type t = int * int * int [@@deriving compare, hash, equal, sexp_of]

  let distance ((a, b, c), (x, y, z)) = sq (x - a) + sq (y - b) + sq (z - c)
end

let min_exn ~compare l =
  List.zip_exn l (List.range 0 (List.length l))
  |> List.min_elt ~compare:(Tuple2.compare ~cmp1:compare ~cmp2:Int.compare)
  |> Option.value_exn
  |> fst
;;

module Adj = struct
  (* type t = (Point3.t, Point3.t list) Hashtbl.t *)

  let group t a =
    let seen = Hash_set.create (module Point3) in
    let rec aux coord =
      if Hash_set.mem seen coord
      then ()
      else (
        Hash_set.add seen coord;
        match Hashtbl.find t coord with
        | None -> ()
        | Some neighbors -> List.iter neighbors ~f:aux)
    in
    aux a;
    Hash_set.to_list seen |> List.sort ~compare:Point3.compare
  ;;

  let join t a b =
    Hashtbl.add_multi t ~key:a ~data:b;
    Hashtbl.add_multi t ~key:b ~data:a
  ;;

  let is_connected t a b =
    match Hashtbl.find t a with
    | None -> false
    | Some b' -> List.mem ~equal:Point3.equal b' b
  ;;
end

let part1 coords =
  let all_pairs = all_pairs coords |> Sequence.to_list in
  let adj = Hashtbl.create (module Point3) in
  for _ = 1 to 1000 do
    let a, b =
      all_pairs
      |> List.filter ~f:(fun (a, b) -> not (Adj.is_connected adj a b))
      |> min_exn ~compare:(Comparable.lift Int.compare ~f:Point3.distance)
    in
    print_s [%message "connecting" (a : Point3.t) (b : Point3.t)];
    Adj.join adj a b
  done;
  let groups =
    List.map coords ~f:(fun coord -> Adj.group adj coord)
    |> List.dedup_and_sort ~compare:[%compare: Point3.t list]
    |> List.map ~f:List.length
    |> List.sort ~compare:(Comparable.reverse Int.compare)
    |> (fun l -> List.take l 3)
    |> List.reduce_exn ~f:( * )
  in
  print_int groups;
  ()
;;

let part2 coords =
  let all_pairs = all_pairs coords |> Sequence.to_list in
  let adj = Hashtbl.create (module Point3) in
  let rec loop () =
    let a, b =
      all_pairs
      |> List.filter ~f:(fun (a, b) -> not (Adj.is_connected adj a b))
      |> min_exn ~compare:(Comparable.lift Int.compare ~f:Point3.distance)
    in
    print_s [%message "connecting" (a : Point3.t) (b : Point3.t)];
    Adj.join adj a b;
    let group_size = Adj.group adj a |> List.length in
    print_s [%message (group_size : int)];
    if group_size = List.length coords
    then (
      let (ax, _, _), (bx, _, _) = a, b in
      print_int (ax * bx))
    else loop ()
  in
  loop ()
;;

let parse =
  let open Angstrom in
  let coord = lift3 Tuple3.create (integer <* char ',') (integer <* char ',') integer in
  many_lines_of coord |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

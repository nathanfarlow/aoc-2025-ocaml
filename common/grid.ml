open! Core

type 'a t = 'a array array [@@deriving sexp_of]

let create xs = List.map xs ~f:Array.of_list |> Array.of_list
let height = Array.length
let width t = Array.length t.(0)
let get t (i, j) = t.(i).(j)
let in_bounds t (i, j) = i >= 0 && i < height t && j >= 0 && j < width t
let get_opt t (i, j) = if in_bounds t (i, j) then Some t.(i).(j) else None
let set t (i, j) v = t.(i).(j) <- v
let set_opt t (i, j) v = if in_bounds t (i, j) then t.(i).(j) <- v

let neighbors4 grid (i, j) =
  [ i - 1, j; i + 1, j; i, j - 1; i, j + 1 ]
  |> List.filter_map ~f:(fun pos ->
    get_opt grid pos |> Option.map ~f:(fun val_ -> pos, val_))
;;

let neighbors8 grid (i, j) =
  [ i - 1, j - 1
  ; i - 1, j
  ; i - 1, j + 1
  ; i, j - 1
  ; i, j + 1
  ; i + 1, j - 1
  ; i + 1, j
  ; i + 1, j + 1
  ]
  |> List.filter_map ~f:(fun pos ->
    get_opt grid pos |> Option.map ~f:(fun val_ -> pos, val_))
;;

let find_all ~f grid =
  let locs = ref [] in
  Array.iteri grid ~f:(fun i ->
    Array.iteri ~f:(fun j cell -> if f (i, j) cell then locs := (i, j) :: !locs));
  List.rev !locs
;;

let count ~f grid = find_all ~f grid |> List.length

let find_opt ~f grid =
  Array.find_mapi grid ~f:(fun i ->
    Array.find_mapi ~f:(fun j cell -> Option.some_if (f (i, j) cell) (i, j)))
;;

let find_exn ~f grid = find_opt ~f grid |> Option.value_exn
let copy grid = Array.map grid ~f:Array.copy |> Array.copy
let equal equal a b = Array.equal (Array.equal equal) a b

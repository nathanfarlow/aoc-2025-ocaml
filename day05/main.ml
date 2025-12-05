open! Core
open! Common

let in_range id (start, end_) = start <= id && id <= end_

let part1 (ranges, available_ids) =
  List.count available_ids ~f:(fun id -> List.exists ranges ~f:(in_range id)) |> print_int
;;

type state =
  { total : int
  ; depth : int
  ; start : int
  }
[@@deriving sexp_of]

type event =
  | Start
  | End
[@@deriving compare]

let part2 (ranges, _) =
  List.concat_map ranges ~f:(fun (start, end_) -> [ start, Start; end_, End ])
  |> List.sort ~compare:[%compare: int * event]
  |> List.fold ~init:None ~f:(fun state (t, event) ->
    match event with
    | Start ->
      (match state with
       | None -> Some { total = 0; depth = 1; start = t }
       | Some state ->
         if state.depth = 0
         then Some { state with start = t; depth = 1 }
         else Some { state with depth = state.depth + 1 })
    | End ->
      let state = Option.value_exn state in
      if state.depth = 1
      then Some { state with depth = 0; total = state.total + t - state.start + 1 }
      else Some { state with depth = state.depth - 1 })
  |> Option.iter ~f:(fun state -> print_int state.total)
;;

let parse =
  let open Angstrom in
  let ranges =
    let range = both (integer <* char '-') integer in
    many_lines_of range
  in
  let available_ids = many_lines_of integer in
  both (ranges <* string "\n\n") available_ids |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

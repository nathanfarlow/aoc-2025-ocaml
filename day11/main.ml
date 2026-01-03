open! Core
open! Common

let part1 (data : (string * string list) list) =
  let dag = Hashtbl.create (module String) in
  List.iter data ~f:(fun (node, edges) -> Hashtbl.set dag ~key:node ~data:edges);
  let num =
    Memo.recursive ~hashable:String.hashable (fun num node ->
      match node with
      | "out" -> 1
      | _ -> Hashtbl.find dag node |> Option.value ~default:[] |> sum ~f:num)
  in
  print_int (num "you")
;;

let part2 _ = failwith ""

let parse =
  let open Angstrom in
  let line =
    lift2
      Tuple2.create
      (take_till_p (string ": "))
      (sep_by1 (char ' ') (take_while1 (Fn.non Char.is_whitespace)))
  in
  many_lines_of line |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

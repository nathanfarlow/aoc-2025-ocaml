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

module Key = struct
  module T = struct
    type t =
      { fft : bool
      ; dac : bool
      ; node : string
      }
    [@@deriving hash, compare, sexp]
  end

  include T
  include Hashable.Make (T)
end

let part2 (data : (string * string list) list) =
  let dag = Hashtbl.create (module String) in
  List.iter data ~f:(fun (node, edges) -> Hashtbl.set dag ~key:node ~data:edges);
  let num =
    Memo.recursive ~hashable:Key.hashable (fun num key ->
      match key.node with
      | "out" -> if key.fft && key.dac then 1 else 0
      | _ ->
        let fft = key.fft || String.equal key.node "fft" in
        let dac = key.dac || String.equal key.node "dac" in
        Hashtbl.find dag key.node
        |> Option.value ~default:[]
        |> List.map ~f:(fun node -> { Key.fft; dac; node })
        |> sum ~f:num)
  in
  print_int (num { node = "svr"; fft = false; dac = false })
;;

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

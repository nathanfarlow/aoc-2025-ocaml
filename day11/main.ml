open! Core
open! Common

let part1 dag =
  let num_paths =
    Memo.recursive ~hashable:String.hashable (fun num_paths node ->
      match node with
      | "out" -> 1
      | _ -> Map.find dag node |> Option.value ~default:[] |> sum ~f:num_paths)
  in
  print_int (num_paths "you")
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

let part2 dag =
  let num_paths =
    Memo.recursive ~hashable:Key.hashable (fun num_paths key ->
      match key.node with
      | "out" -> if key.fft && key.dac then 1 else 0
      | _ ->
        Map.find dag key.node
        |> Option.value ~default:[]
        |> List.map ~f:(fun node ->
          let fft = key.fft || String.equal key.node "fft" in
          let dac = key.dac || String.equal key.node "dac" in
          { Key.fft; dac; node })
        |> sum ~f:num_paths)
  in
  print_int (num_paths { node = "svr"; fft = false; dac = false })
;;

let parse =
  let open Angstrom in
  let line =
    lift2
      Tuple2.create
      (take_till_p (string ": "))
      (sep_by1 (char ' ') (take_while1 (Fn.non Char.is_whitespace)))
  in
  many_lines_of line >>| Map.of_alist_exn (module String) |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

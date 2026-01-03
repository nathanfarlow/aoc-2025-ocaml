open! Core
open! Common

module M = struct
  type t = bool list [@@deriving hash, compare, sexp]
end

let toggle state button =
  let state = Array.of_list state in
  List.iter button ~f:(fun i -> state.(i) <- not state.(i));
  Array.to_list state
;;

let fewest_presses (target, buttons, _) =
  let seen = Hash_set.create (module M) in
  let queue = Queue.create () in
  Queue.enqueue queue (0, List.init (List.length target) ~f:(fun _ -> false));
  let rec go () =
    let depth, value = Queue.dequeue_exn queue in
    if List.equal Bool.equal value target
    then depth
    else (
      List.iter buttons ~f:(fun button ->
        let toggled = toggle value button in
        if not (Hash_set.mem seen toggled) then Queue.enqueue queue (depth + 1, toggled));
      go ())
  in
  go ()
;;

let part1 = sum ~f:fewest_presses >> print_int

let part2 (_, buttons, targets) =
  let open Lp in
  let targets = List.map targets ~f:(Int.to_float >> c) in
  let vectors =
    List.map buttons ~f:(fun button ->
      List.init (List.length targets) ~f:(fun i ->
        if List.mem button i ~equal:Int.equal then c 1. else c 0.))
  in
  let vars =
    List.init (List.length buttons) ~f:(Int.to_string >> ( ^ ) "b" >> Lp.var ~integer:true)
  in
  let obj = minimize (List.reduce_exn vars ~f:( ++ )) in
  let constraints =
    List.zip_exn vars vectors
    |> List.map ~f:(fun (var, vector) -> List.map vector ~f:(( *~ ) var))
    |> List.transpose_exn
    |> List.map ~f:(List.reduce_exn ~f:( ++ ))
    |> List.zip_exn targets
    |> List.map ~f:(Tuple2.uncurry eq)
  in
  Lp_glpk.solve ~term_output:false (make obj constraints)
  |> function
  | Ok (obj, _) -> Float.iround_nearest_exn obj
  | _ -> assert false
;;

let part2 = sum ~f:part2 >> print_int

let parse =
  let open Angstrom in
  let ints = sep_by1 (char ',') integer in
  let target =
    let light = char '.' *> return false <|> char '#' *> return true in
    char '[' *> many1 light <* char ']'
  in
  let buttons =
    let button = char '(' *> ints <* char ')' in
    sep_by1 (char ' ') button
  in
  let joltages = char '{' *> ints <* char '}' in
  let machine = lift3 Tuple3.create (target <* space) (buttons <* space) joltages in
  many_lines_of machine |> exec_exn
;;

let () = run_with_input_file ~part1 ~part2 ~parse

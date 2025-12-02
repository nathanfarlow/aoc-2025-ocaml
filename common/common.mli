open! Core
module Math = Math
module Grid = Grid
module Point = Point

module Syntax : sig
  include module type of Option.Let_syntax

  val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

include module type of Syntax

module Angstrom : sig
  include module type of Angstrom with type 'a t = 'a Angstrom.t
  include module type of Angstrom.Let_syntax

  (** Space or tab *)
  val space : unit t

  (** 0 through 9 *)
  val digit : int t

  val integer : int t

  (** Skip all characters until we encounter the given parser. *)
  val skip_till : 'a t -> 'a t

  (** Skips [Char.is_whitespace] *)
  val ws : unit t

  val many_lines_of : 'a t -> 'a list t
  val exec_exn : ?consume:Consume.t -> 'a t -> string -> 'a
  val exec_opt : ?consume:Consume.t -> 'a t -> string -> 'a option
end

(** Equivalent to
    {v
       for i in range(len(l)):
        for j in range(i + 1, len(l)):
          yield l[i], l[j:]
    v}
*)
val triangular : 'a list -> ('a * 'a list) Sequence.t

(** Equivalent to
    {v
      for i in range(len(l)):
        for j in range(i + 1, len(l)):
          yield l[i], l[j]
    v}
 *)
val all_pairs : 'a list -> ('a * 'a) Sequence.t

(** Equivalent to
    {v
      list(zip(l, l[1:]))
    v}
 *)
val zip_next : 'a list -> ('a * 'a) list

val sum : f:('a -> int) -> 'a list -> int
val print_int : int -> unit

val run_with_input_file
  :  part1:('a -> unit)
  -> part2:('a -> unit)
  -> parse:(string -> 'a)
  -> unit

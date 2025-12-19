(** A 2D array with bounds checking. *)

open! Core

type 'a t = 'a array array [@@deriving sexp_of]

val create : 'a list list -> 'a t
val init : h:int -> w:int -> f:(Point.t -> 'a) -> 'a t
val height : 'a t -> int
val width : 'a t -> int
val in_bounds : 'a t -> Point.t -> bool
val get : 'a t -> Point.t -> 'a
val get_opt : 'a t -> Point.t -> 'a option
val set : 'a t -> Point.t -> 'a -> unit
val set_opt : 'a t -> Point.t -> 'a -> unit
val neighbors4 : 'a t -> Point.t -> (Point.t * 'a) list
val neighbors8 : 'a t -> Point.t -> (Point.t * 'a) list
val count : f:(Point.t -> 'a -> bool) -> 'a t -> int
val find_all : f:(Point.t -> 'a -> bool) -> 'a t -> Point.t list
val find_opt : f:(Point.t -> 'a -> bool) -> 'a t -> Point.t option
val find_exn : f:(Point.t -> 'a -> bool) -> 'a t -> Point.t
val copy : 'a t -> 'a t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

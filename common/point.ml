open! Core

type t = int * int

module O = struct
  let lift f (a, b) (c, d) = f a c, f b d
  let ( + ) = lift ( + )
  let ( - ) = lift ( - )
  let ( * ) = lift ( * )
end

include Tuple.Comparable (Int) (Int)
include Tuple.Hashable (Int) (Int)

let up = -1, 0
let down = 1, 0
let left = 0, -1
let right = 0, 1
let neighbors4 (i, j) = [ i - 1, j; i + 1, j; i, j - 1; i, j + 1 ]

let neighbors8 (i, j) =
  [ i - 1, j - 1
  ; i - 1, j
  ; i - 1, j + 1
  ; i, j - 1
  ; i, j + 1
  ; i + 1, j - 1
  ; i + 1, j
  ; i + 1, j + 1
  ]
;;

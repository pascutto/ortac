exception Exit

type t = { mutable x : int; y : bool }

let get_x t = t.x

let negate_x t = { t with x = -t.x }

let negate_fail t =
  t.x <- -t.x;
  raise Exit

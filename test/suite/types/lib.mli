exception Exit

type t = { mutable x : int; y : bool }
(*@ invariant x >= 0 *)

val get_x : t -> int
(*@ r = get_x t
    ensures r = t.x *)

val negate_x : t -> t
(*@ r = negate_x t
    ensures r.x = -t.x *)

val negate_fail : t -> t
(*@ r = negate_fail t *)

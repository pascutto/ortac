module W = Warnings
open Ppxlib
open Gospel

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_setup : location -> string -> (expression -> expression) * string

val axiom_definition :
  driver:Drv.t -> register_name:string -> Tterm.term -> Translated.term

val with_pres :
  driver:Drv.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.value ->
  Translated.value

val with_posts :
  driver:Drv.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.value ->
  Translated.value

val with_xposts :
  driver:Drv.t ->
  term_printer:(Tterm.term -> string) ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  Translated.value ->
  Translated.value

(* val mk_call : *)
(*   driver:Drv.t -> *)
(*   register_name:expression -> *)
(*   term_printer:(Tterm.term -> string) -> *)
(*   pattern -> *)
(*   location -> *)
(*   label -> *)
(*   (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list -> *)
(*   (arg_label * expression) list -> *)
(*   expression -> *)
(*   expression *)

val mk_function_def : driver:Drv.t -> Tterm.term -> expression option

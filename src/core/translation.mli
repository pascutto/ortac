open Ppxlib
open Gospel

exception Unsupported of Location.t option * string

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_setup : location -> string -> (expression -> expression) * string

val mk_pre_checks :
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_call :
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  pattern ->
  location ->
  label ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  (arg_label * expression) list ->
  (expression -> expression) ->
  (negative:bool -> Parsetree.expression -> Parsetree.expression) ->
  bool ->
  expression ->
  expression

val mk_post_checks :
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_invariant_checks :
  state:expression ->
  typ:string ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression

val mk_checks_decl :
  names:string list ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_checks_checks :
  negative:bool ->
  names:string list ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

module W = Warnings
open Gospel

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

val with_constant_checks :
  driver:Drv.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.constant ->
  Translated.constant

val with_xposts :
  driver:Drv.t ->
  term_printer:(Tterm.term -> string) ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  Translated.value ->
  Translated.value

val function_definition : driver:Drv.t -> Tterm.term -> Translated.term

val axiom_definition :
  driver:Drv.t -> register_name:string -> Tterm.term -> Translated.term

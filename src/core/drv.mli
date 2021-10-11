open Gospel

type t

val init : string -> Tmodule.namespace -> t

val add_translation : t -> Tterm.lsymbol -> string -> unit

val remove_translation : t -> Tterm.lsymbol -> unit

val translate : t -> Tterm.lsymbol -> string option

val get_ls : t -> string list -> Tterm.lsymbol

val get_ts : t -> string list -> Ttypes.tysymbol

val signature :
  runtime:string ->
  module_name:string ->
  Gospel.Tmodule.namespace ->
  Gospel.Tast.signature_item list ->
  Ppxlib.structure

val report :
  string -> Gospel.Tmodule.namespace -> Gospel.Tast.signature_item list -> unit

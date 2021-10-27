let run f path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  f ~module_name (List.hd env) sigs

let generate path output =
  let output = Format.formatter_of_out_channel output in
  let f = Ortac_core.Ortac.signature ~runtime:"Ortac_runtime" in
  run f path |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure

let report = run Ortac_core.Ortac.report

type frontend = Default | Monolith

let get_channel = function None -> stdout | Some path -> open_out path

let frontend_printer ppf = function
  | Default -> Fmt.string ppf "Default"
  | Monolith -> Fmt.string ppf "Monolith"

let frontend_parser = function
  | "default" -> Ok Default
  | "monolith" -> Ok Monolith
  | s -> Error (`Msg (Fmt.str "Error: `%s' is not a valid argument" s))

let generate frontend input output () =
  let channel = get_channel output in
  try
    match frontend with
    | Default -> Ortac_default.generate input channel
    | Monolith -> Ortac_monolith.generate input channel
  with Gospel.Warnings.Error e ->
    Fmt.epr "%a@." Gospel.Warnings.pp e;
    exit 1

open Cmdliner

let setup_log =
  let init style_renderer = Fmt_tty.setup_std_outputs ?style_renderer () in
  Term.(const init $ Fmt_cli.style_renderer ())

let output_file =
  let parse s =
    match Sys.is_directory s with
    | true -> Error (`Msg (Fmt.str "Error: `%s' is a directory" s))
    | false | (exception Sys_error _) -> Ok (Some s)
  in
  Arg.(
    value
    & opt (conv ~docv:"OUTPUT" (parse, Fmt.(option string))) None
    & info [ "o"; "output" ] ~docv:"OUTPUT")

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s || Filename.extension s <> ".mli" then
          `Error (Fmt.str "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Fmt.str "Error: `%s' not found" s)
  in
  Arg.(required & pos 0 (some (parse, Fmt.string)) None & info [] ~docv:"FILE")

let frontend =
  Arg.(
    value
    & opt (conv ~docv:"FRONTEND" (frontend_parser, frontend_printer)) Default
    & info [ "f"; "frontend" ] ~docv:"FRONTEND")

let gen =
  let doc = "Run ORTAC code generation." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "gen" ~doc ~version in
  let term =
    Term.(const generate $ frontend $ ocaml_file $ output_file $ setup_log)
  in
  Cmd.v info term

let report =
  let doc = "Ortac report." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "report" ~doc ~version in
  Cmd.v info Term.(const Ortac_default.report $ ocaml_file)

let main =
  let doc = "Ortac command line tool." in
  let version = "ortac version %%VERSION%%" in
  let info = Cmd.info "ortac" ~doc ~version in
  Cmd.group info [ gen; report ]

let () = exit @@ Cmd.eval @@ main

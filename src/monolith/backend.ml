open Ppxlib

module M : Ortac_core.Backend.S = struct
  let prelude =
    let loc = Location.none in
    [ [%stri open Ortac_runtime] ]
end

let loc = Location.none

module G = Ortac_core.Ortac.Make (M)
module A = Ast_builder.Default
module B = Ortac_core.Builder

let mk_reference rtac =
  let module_r = A.pmod_structure ~loc rtac in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "R")) ~expr:module_r
  in
  A.pstr_module ~loc module_bind

let mk_candidate module_name =
  let module_c = A.pmod_ident ~loc (B.lident module_name) in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "C")) ~expr:module_c
  in
  A.pstr_module ~loc module_bind

let is_arrow = function Ptyp_arrow _ -> true | _ -> false

let constructible_int =
  [%expr int_within (Gen.semi_open_interval Int.min_int Int.max_int)]

let rec translate_ret s =
  match s.ptyp_desc with
  | Ptyp_var s -> B.evar s
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr int]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ param ]) ->
      [%expr list [%e translate_ret param]]
  | _ -> failwith "not implemented yet"

let rec traduction s =
  match s.ptyp_desc with
  | Ptyp_var s -> B.evar s
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> constructible_int
  | Ptyp_arrow (_, x, y) when is_arrow y.ptyp_desc ->
      [%expr [%e traduction x] ^> [%e traduction y]]
  | Ptyp_arrow (_, x, y) -> [%expr [%e traduction x] ^!> [%e translate_ret y]]
  | _ -> failwith "not implemented yet"

let mk_declaration (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_val (decl, _ghost) ->
      let fun_name = decl.vd_name.id_str in
      let fun_type = decl.vd_type in
      let msg = B.estring (Printf.sprintf "%s is Ok" fun_name) in
      let reference = Printf.sprintf "R.%s" fun_name in
      let candidate = Printf.sprintf "C.%s" fun_name in
      Some
        [%expr
          let spec = [%e traduction fun_type] in
          declare [%e msg] spec [%e B.evar reference] [%e B.evar candidate]]
  | _ -> None

let mk_declarations s =
  match List.filter_map mk_declaration s with
  | [] -> raise (failwith "module is empty")
  | [ e ] -> [%stri let () = [%e e]]
  | e1 :: es -> [%stri let () = [%e List.fold_left B.pexp_sequence e1 es]]

let mk_specs s =
  let main =
    [%stri
      let () =
        let fuel = 10 in
        main fuel]
  in
  [ mk_declarations s; main ]

let standalone module_name s =
  let mod_ref = mk_reference (G.signature module_name s) in
  let mod_can = mk_candidate module_name in
  let specs = mk_specs s in
  [%stri open Monolith] :: mod_ref :: mod_can :: specs

let generate path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> standalone module_name
  |> Ppxlib_ast.Pprintast.structure Fmt.stdout

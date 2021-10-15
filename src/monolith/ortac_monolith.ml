module W = Warnings
open Ppxlib
open Gospel

module M : Ortac_core.Frontend.S = struct
  let prelude =
    let loc = Location.none in
    [
      [%stri
        module Errors = struct
          type t = Ortac_runtime.error_report

          let create loc fun_name =
            { Ortac_runtime.loc; Ortac_runtime.fun_name; errors = [] }

          let register t e = Ortac_runtime.(t.errors <- e :: t.errors)

          let is_pre = function
            | Ortac_runtime.Violated_condition e -> e.term_kind = Pre
            | Ortac_runtime.Specification_failure e -> e.term_kind = Pre
            | _ -> false

          let report t =
            let open Ortac_runtime in
            match t.errors with
            | [] -> ()
            | errs when List.exists is_pre errs -> raise Monolith.PleaseBackOff
            | _ ->
                Fmt.flush Fmt.stderr (pp_error_report Fmt.stderr t);
                (* pp_error_report Fmt.stderr t; *)
                raise (Error t)
        end];
    ]
end

module G = Ortac_core.Ortac.Make (M)
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none
let unsupported msg loc = raise (W.Error (W.MonolithSpec msg, loc))

let mk_reference module_name env s =
  let rtac = G.signature module_name env s in
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

let rec ty2spec drv (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar _ -> [%expr sequential ()]
  | Tyapp (ts, tl) -> tyapp2spec drv ts tl

and tyapp2spec drv (ts : Ttypes.tysymbol) (tl : Ttypes.ty list) =
  let get_ts = Ortac_core.Drv.get_ts drv in
  if Ttypes.ts_equal ts Ttypes.ts_unit then [%expr unit]
  else if Ttypes.ts_equal ts Ttypes.ts_char then [%expr char]
  else if Ttypes.ts_equal ts Ttypes.ts_integer then [%expr int]
  else if Ttypes.ts_equal ts Ttypes.ts_string then [%expr string]
  else if Ttypes.ts_equal ts Ttypes.ts_list && List.length tl = 1 then
    [%expr list [%e ty2spec drv (List.hd tl)]]
  else if Ttypes.ts_equal ts (get_ts [ "Gospelstdlib"; "int" ]) then [%expr int]
  else if
    Ttypes.ts_equal ts (get_ts [ "Gospelstdlib"; "array" ])
    && List.length tl = 1
  then [%expr array [%e ty2spec drv (List.hd tl)]]
  else if Ttypes.is_ts_tuple ts && List.length tl = 2 then
    let a = (Nolabel, ty2spec drv (List.hd tl)) in
    let b = (Nolabel, ty2spec drv (List.nth tl 1)) in
    B.pexp_apply [%expr ( *** )] [ a; b ]
  else unsupported ts.ts_ident.id_str ts.ts_ident.id_loc

let translate drv (lb_arg : Tast.lb_arg) =
  match lb_arg with
  | Lunit -> [%expr unit]
  | _ ->
      let vs = Tast.vs_of_lb_arg lb_arg in
      let tn = vs.Tterm.vs_ty in
      ty2spec drv tn

let spec drv args ret =
  let arr = [%expr ( ^> )] in
  let xarr = [%expr ( ^!> )] in
  let args = List.map (translate drv) args in
  let ret =
    match ret with
    | [] -> assert false
    | [ r ] -> translate drv r
    | [ r1; r2 ] ->
        let a = (Nolabel, translate drv r1) in
        let b = (Nolabel, translate drv r2) in
        B.pexp_apply [%expr ( *** )] [ a; b ]
    | _ -> unsupported "tuple bigger than pair" loc
  in
  let mk_arrow a img dom = B.pexp_apply a [ (Nolabel, img); (Nolabel, dom) ] in
  let rec spec = function
    | [] -> assert false
    | [ arg ] -> mk_arrow xarr arg ret
    | arg :: arg' :: args -> mk_arrow arr arg (spec (arg' :: args))
  in
  spec args

let mk_declaration drv (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_val (decl, _ghost) ->
      if decl.vd_spec <> None then (
        try
          let fun_name = decl.vd_name.id_str in
          let spec = spec drv decl.vd_args decl.vd_ret in
          let msg = B.estring (Printf.sprintf "%s is Ok" fun_name) in
          let reference = Printf.sprintf "R.%s" fun_name in
          let candidate = Printf.sprintf "C.%s" fun_name in
          Some
            [%expr
              let spec = [%e spec] in
              declare [%e msg] spec [%e B.evar reference] [%e B.evar candidate]]
        with W.Error e ->
          W.register e;
          None)
      else None
  | _ -> None

let mk_declarations drv s =
  match List.filter_map (mk_declaration drv) s with
  | [] ->
      W.report ();
      raise (failwith "module is empty")
  | [ e ] -> [%stri let () = [%e e]]
  | e1 :: es -> [%stri let () = [%e List.fold_left B.pexp_sequence e1 es]]

let mk_specs drv s =
  let main =
    [%stri
      let () =
        let fuel = 100 in
        main fuel]
  in
  [ mk_declarations drv s; main ]

let standalone module_name env s =
  let driver = Ortac_core.Drv.init module_name env in
  let module_r = mk_reference module_name env s in
  let module_c = mk_candidate module_name in
  let module_g = Generators.generators driver s in
  let module_p = Printers.printers driver s in
  let module_s = Spec.specs s in
  let specs = mk_specs driver s in
  [%stri open Monolith]
  :: [%stri module M = Ortac_runtime_monolith]
  :: module_r
  :: module_c
  :: module_g
  :: module_p
  :: module_s
  :: specs

let generate path output =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  let output = Format.formatter_of_out_channel output in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  standalone module_name (List.hd env) sigs
  |> Fmt.pf output "%a@." Ppxlib_ast.Pprintast.structure;
  W.report ();
  Ortac_core.Warnings.report ()

module W = Warnings
open Ppxlib
open Gospel
open Translated

module Make (B : Frontend.S) = struct
  open Builder
  module T = Translation

  let register_name = gen_symbol ~prefix:"__error"

  let term_printer text global_loc (t : Tterm.term) =
    match t.t_loc with
    | None -> Fmt.str "%a" Tterm.print_term t
    | Some loc -> (
        try
          String.sub text
            (loc.loc_start.pos_cnum - global_loc.loc_start.pos_cnum)
            (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
        with Invalid_argument _ -> Fmt.str "%a" Tterm.print_term t)

  let var_of_arg ~driver:_ arg =
    let name =
      match arg with
      | Tast.Lunit -> "()"
      | _ ->
          let vs = Tast.vs_of_lb_arg arg in
          Fmt.str "%a" Tast.Ident.pp vs.vs_name
    in
    let type_ = Tast.ty_of_lb_arg arg in
    let invariants = assert false (* use the driver to find invariants *) in
    { name; type_; invariants }

  let type_ ~driver ~ghost (td : Tast.type_declaration) =
    let name = td.td_ts.ts_ident.id_str in
    let loc = td.td_loc in
    let mutable_ = assert false (* infer if the type is mutable or not *) in
    let type_ = type_ ~name ~loc ~mutable_ ~ghost in
    let process ~type_ (spec : Tast.type_spec) =
      (* let term_printer = Fmt.str "%a" Tterm.print_term in *)
      let mutable_ = type_.mutable_ || spec.ty_ephemeral in
      let type_ =
        type_
        |> T.with_models ~driver spec.ty_fields
        |> T.with_invariants ~driver spec.ty_invariants
      in
      { type_ with mutable_ }
    in
    Option.fold ~none:type_ ~some:(process ~type_) td.td_spec

  let value ~driver ~ghost (vd : Tast.val_description) =
    let name = vd.vd_name.id_str in
    let loc = vd.vd_loc in
    let register_name = register_name () in
    let arguments = List.map (var_of_arg ~driver) vd.vd_args in
    let returns = List.map (var_of_arg ~driver) vd.vd_ret in
    let pure = false in
    let value =
      value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost
    in
    let process ~value (spec : Tast.val_spec) =
      let term_printer = term_printer spec.sp_text spec.sp_loc in
      let value =
        value
        |> T.with_pres ~driver ~term_printer spec.sp_pre
        |> T.with_posts ~driver ~term_printer spec.sp_post
        |> T.with_xposts ~driver ~term_printer spec.sp_xpost
      in
      { value with pure = spec.sp_pure }
    in
    Option.fold ~none:value ~some:(process ~value) vd.vd_spec

  let constant ~driver ~ghost (vd : Tast.val_description) =
    let name = vd.vd_name.id_str in
    let loc = vd.vd_loc in
    let register_name = register_name () in
    let type_ =
      assert (List.length vd.vd_ret = 1);
      Tast.ty_of_lb_arg (List.hd vd.vd_ret)
    in
    let constant = constant ~name ~loc ~register_name ~type_ ~ghost in
    let process ~constant (spec : Tast.val_spec) =
      let term_printer = term_printer spec.sp_text spec.sp_loc in
      constant |> T.with_constant_checks ~driver ~term_printer spec.sp_post
    in
    Option.fold ~none:constant ~some:(process ~constant) vd.vd_spec

  let function_ ~driver (func : Tast.function_) : Translated.function_ =
    let name = func.fun_ls.ls_name.id_str in
    let loc = func.fun_loc in
    let definition = Option.map (T.function_definition ~driver) func.fun_def in
    { name; loc; definition }

  let axiom ~driver (ax : Tast.axiom) =
    let name = ax.ax_name.id_str in
    let loc = ax.ax_loc in
    let register_name = register_name () in
    let definition = T.axiom_definition ~driver ~register_name ax.ax_term in
    { name; loc; register_name; definition }

  let signature module_name namespace s =
    let driver = Drv.init module_name namespace in
    List.fold_left
      (fun env (sig_item : Tast.signature_item) ->
        match sig_item.sig_desc with
        | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~driver ~ghost vd
        | Sig_val (vd, ghost) -> constant ~driver ~ghost vd
        | Sig_type (_rec, td, ghost) -> types ~driver ~ghost td
        | Sig_function func -> function_ ~driver func
        | Sig_axiom ax -> axiom ~driver ax
        | _ -> env)
      driver s
end

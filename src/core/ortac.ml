open Ppxlib
open Gospel
open Fmt

module Make (B : Backend.S) = struct
  open Builder
  module T = Translation

  let type_invariants = Hashtbl.create 0

  let of_gospel_args args =
    let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
    let to_ty x = x.Tterm.vs_ty in
    List.fold_right
      (fun arg (eargs, pargs, targs) ->
        match arg with
        | Tast.Lunit ->
            ( (Nolabel, eunit) :: eargs,
              (Nolabel, punit) :: pargs,
              Ttypes.ty_unit :: targs )
        | Tast.Lnone x ->
            let s = to_string x in
            let t = to_ty x in
            ((Nolabel, evar s) :: eargs, (Nolabel, pvar s) :: pargs, t :: targs)
        | Tast.Loptional x ->
            let s = to_string x in
            let t = to_ty x in
            ( (Optional s, evar s) :: eargs,
              (Nolabel, pvar s) :: pargs,
              t :: targs )
        | Tast.Lnamed x ->
            let s = to_string x in
            let t = to_ty x in
            ( (Labelled s, evar s) :: eargs,
              (Labelled s, pvar s) :: pargs,
              t :: targs )
        | Tast.Lghost _ -> (eargs, pargs, targs))
      args ([], [], [])

  let value (val_desc : Tast.val_description) =
    let process (spec : Tast.val_spec) =
      let term_printer (t : Tterm.term) =
        match t.t_loc with
        | None -> Fmt.str "%a" Tterm.print_term t
        | Some loc ->
            String.sub spec.sp_text loc.loc_start.pos_cnum
              (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
      in
      let loc = val_desc.vd_loc in
      if List.length spec.sp_args = 0 then
        raise (T.Unsupported (Some loc, "non-function value"));
      let setup_expr, register_name = T.mk_setup loc val_desc.vd_name.id_str in
      let register_name = evar register_name in
      let eargs, pargs, targs = of_gospel_args spec.sp_args in
      let ret_pat, ret_expr = T.returned_pattern spec.sp_ret in
      let input_invariants_checks next =
        let ts_of_ty ty =
          match ty.Ttypes.ty_node with
          | Ttypes.Tyvar _ -> None
          | Tyapp (ts, _) -> Some ts
        in
        let rec loop e t =
          match (e, t) with
          | [], [] -> next
          | (_, eh) :: et, th :: tt ->
              Option.bind (ts_of_ty th) (Hashtbl.find_opt type_invariants)
              |> Option.map (fun check_function ->
                     let check =
                       eapply check_function [ register_name; evar "Pre"; eh ]
                     in
                     pexp_sequence check (loop et tt))
              |> Option.value ~default:(loop et tt)
          | _, _ -> assert false
        in
        loop eargs targs
      in
      let pre_checks =
        T.mk_pre_checks ~register_name ~term_printer spec.sp_pre
      in
      let let_call =
        T.mk_call ~register_name ~term_printer ret_pat loc
          val_desc.vd_name.id_str spec.sp_xpost eargs
      in
      let post_checks =
        T.mk_post_checks ~register_name ~term_printer spec.sp_post
      in
      let body =
        efun pargs @@ setup_expr @@ input_invariants_checks @@ pre_checks
        @@ let_call @@ post_checks @@ ret_expr
      in
      [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
    in
    Option.map process val_desc.vd_spec

  let typ (t : Tast.type_declaration) =
    let process (spec : Tast.type_spec) =
      if spec.ty_fields <> [] then
        raise (T.Unsupported (Some t.td_loc, "type models"));
      let check_name =
        Fmt.kstr
          (fun prefix -> gen_symbol ~prefix ())
          "__invariant_%s" t.td_ts.ts_ident.id_str
      in
      let term_printer = Fmt.str "%a" Tterm.print_term in
      let register_name = gen_symbol ~prefix:"__register" () in
      let state = gen_symbol ~prefix:"__state" () in
      let typ = gen_symbol ~prefix:"__t" () in
      let body =
        T.mk_invariant_checks ~state:(evar state) ~typ
          ~register_name:(evar register_name) ~term_printer spec.ty_invariants
      in
      Hashtbl.replace type_invariants t.td_ts (evar check_name);
      [%stri
        let [%p pvar check_name] =
         fun [%p pvar register_name] [%p pvar state] [%p pvar typ] -> [%e body]]
    in
    Option.bind t.td_spec (fun s ->
        if s.ty_invariants = [] then None else Some (process s))

  let signature module_name s =
    let declarations =
      List.filter_map
        (fun (sig_item : Tast.signature_item) ->
          match sig_item.sig_desc with
          | Sig_val (decl, false) -> value decl
          | Sig_type (_, [ t ], false) -> typ t
          | _ -> None)
        s
    in
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    B.prelude @ include_lib :: declarations
end

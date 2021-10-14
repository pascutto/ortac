module W = Warnings
open Ppxlib
open Builder
open Gospel
open Translated
module T = Translation

let value ~name ~loc ~arguments ~returns ~register_name ~ghost ~pure =
  {
    name;
    loc;
    arguments;
    returns;
    register_name;
    ghost;
    pure;
    preconditions = [];
    postconditions = [];
    xpostconditions = [];
  }

let setup name loc register_name next =
  [%expr
    let [%p pvar register_name] =
      Ortac_runtime.Errors.create [%e elocation loc] [%e estring name]
    in
    [%e next]]

let value
    {
      name;
      loc;
      arguments;
      returns;
      register_name;
      ghost = _;
      pure = _;
      preconditions;
      postconditions;
      xpostconditions;
    } =
  (*   let body = *)
  (*     efun pargs @@ setup_expr @@ pre_checks @@ let_call @@ post_checks *)
  (*     @@ ret_expr *)
  (*   in *)
  (*   (if spec.sp_pure then *)
  (*    let ls = Drv.get_ls driver [ val_desc.vd_name.id_str ] in *)
  (*    Drv.add_translation driver ls val_desc.vd_name.id_str); *)
  (*   [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]] *)
  (* in *)
  (* Option.map process val_desc.vd_spec *)
  let eargs, pargs =
    List.map
      (fun (var : ocaml_var) ->
        ((var.label, evar var.name), (var.label, pvar var.name)))
      arguments
    |> List.split
  in
  (* XXX FIXME: compute real return pattern XXX *)
  let return = pvar (List.hd returns).name in
  let pres =
    List.filter_map
      (fun (t : term) -> Result.to_option t.translation)
      preconditions
    |> eunit_seq
  in
  let xposts =
    List.filter_map
      (fun (t : xpost) -> Result.to_option t.translation)
      xpostconditions
  in
  let call = pexp_apply (evar name) eargs in
  (* XXX FIXME: pattern match on exceptions XXX *)
  let try_call = pexp_try call (List.hd xposts) in
  let posts =
    List.filter_map
      (fun (t : term) -> Result.to_option t.translation)
      postconditions
    |> eunit_seq
  in
  let setup = setup name loc register_name in
  let body =
    pexp_sequence (setup @@ pres)
      ((fun next ->
         [%expr
           let [%p return] = [%e try_call] in
           [%e next]])
      @@ posts)
  in
  [%stri let [%p pvar name] = [%e efun pargs body]]

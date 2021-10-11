module W = Warnings
open Ppxlib
open Gospel
open Fmt
open Builder
open Translated
module F = Failure

let rec pattern pn =
  let pattern' (p : Tterm.pattern) = pattern p.p_node in
  match pn with
  | Tterm.Pwild -> ppat_any
  | Tterm.Pvar v -> pvar (str "%a" Identifier.Ident.pp v.vs_name)
  | Tterm.Papp (l, pl) when Tterm.is_fs_tuple l ->
      ppat_tuple (List.map pattern' pl)
  | Tterm.Papp (l, pl) ->
      let args =
        if pl = [] then None else Some (ppat_tuple (List.map pattern' pl))
      in
      ppat_construct (lident l.ls_name.id_str) args
  | Tterm.Por (p1, p2) -> ppat_or (pattern' p1) (pattern' p2)
  | Tterm.Pas (p, v) ->
      ppat_alias (pattern' p) (noloc (str "%a" Identifier.Ident.pp v.vs_name))

type bound = Inf of expression | Sup of expression

let rec bounds ~driver ~loc (var : Tterm.vsymbol) (t1 : Tterm.term)
    (t2 : Tterm.term) : expression * expression =
  let unsupported () =
    raise (W.Error (Unsupported "ill formed quantification", loc))
  in
  (* [comb] extracts a bound from an the operator [f] and expression [e].
     [right] indicates if [e] is on the right side of the operator. *)
  let comb ~right (f : Tterm.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then Inf e else Sup e
    | "infix <=" -> if right then Sup e else Inf e
    | "infix <" -> if right then Sup (epred e) else Inf (esucc e)
    | "infix >" -> if right then Inf (esucc e) else Sup (epred e)
    | _ -> unsupported ()
  in
  let bound = function
    | Tterm.Tapp (f, [ { t_node = Tvar vs; _ }; t ])
      when vs.vs_name = var.vs_name ->
        comb ~right:true f (unsafe_term ~driver t)
    | Tterm.Tapp (f, [ t; { t_node = Tvar vs; _ } ])
      when vs.vs_name = var.vs_name ->
        comb ~right:false f (unsafe_term ~driver t)
    | _ -> unsupported ()
  in
  match (bound t1.t_node, bound t2.t_node) with
  | Inf start, Sup stop | Sup stop, Inf start -> (start, stop)
  | _ -> unsupported ()

and unsafe_term ~driver (t : Tterm.term) : expression =
  let term = unsafe_term ~driver in
  let loc = Option.value ~default:Location.none t.t_loc in
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> evar (str "%a" Identifier.Ident.pp vs_name)
  | Tconst c -> econst c
  | Tfield (t, f) -> pexp_field (term t) (lident f.ls_name.id_str)
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_true) -> [%expr true]
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_false) -> [%expr false]
  | Tapp (fs, tlist) when Tterm.is_fs_tuple fs ->
      List.map term tlist |> pexp_tuple
  | Tapp (ls, tlist) when Tterm.(ls_equal ls fs_apply) ->
      let f, args =
        match tlist with
        | [] -> assert false
        | x :: xs -> (term x, List.map term xs)
      in
      eapply f args
  | Tapp (ls, tlist) -> (
      Drv.translate driver ls |> function
      | Some f -> eapply (evar f) (List.map term tlist)
      | None ->
          let func = ls.ls_name.id_str in
          if ls.ls_constr then
            (if tlist = [] then None
            else Some (List.map term tlist |> pexp_tuple))
            |> pexp_construct (lident func)
          else kstr unsupported "function application `%s`" func)
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tlet (x, t1, t2) ->
      let x = str "%a" Identifier.Ident.pp x.vs_name in
      [%expr
        let [%p pvar x] = [%e term t1] in
        [%e term t2]]
  | Tcase (t, ptl) ->
      List.map
        (fun (p, t) ->
          case ~guard:None ~lhs:(pattern p.Tterm.p_node) ~rhs:(term t))
        ptl
      |> pexp_match (term t)
  | Tquant (Tterm.Tlambda, args, _, t) ->
      let t = term t in
      let args =
        List.map
          (fun (vs : Tterm.vsymbol) ->
            (Nolabel, pvar (str "%a" Identifier.Ident.pp vs.vs_name)))
          args
      in
      efun args t
  | Tquant
      ( (Tterm.(Tforall | Texists) as quant),
        [ var ],
        _,
        Tterm.
          {
            t_node =
              Tbinop
                ( ((Timplies | Tand | Tand_asym) as op),
                  { t_node = Tbinop (Tand, t1, t2); _ },
                  p );
            _;
          } ) ->
      (match (quant, op) with
      | Tforall, Timplies | Texists, (Tand | Tand_asym) -> ()
      | _, _ -> unsupported "ill formed quantification");
      let start, stop = bounds ~driver ~loc var t1 t2 in
      let p = term p in
      let quant =
        evar
          (if quant = Tforall then "Ortac_runtime.Z.forall"
          else "Ortac_runtime.Z.exists")
      in
      let x = str "%a" Identifier.Ident.pp var.vs_name in
      let func = pexp_fun Nolabel None (pvar x) p in
      eapply quant [ start; stop; func ]
  | Tquant (_, _, _, _) -> unsupported "quantification"
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] && [%e evar vt2]]
      | Tterm.Tand_asym -> [%expr [%e term t1] && [%e term t2]]
      | Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] || [%e evar vt2]]
      | Tterm.Tor_asym -> [%expr [%e term t1] || [%e term t2]]
      | Tterm.Timplies -> [%expr (not [%e term t1]) || [%e term t2]]
      | Tterm.Tiff -> [%expr [%e term t1] = [%e term t2]])
  | Tnot t -> [%expr not [%e term t]]
  | Told _ -> unsupported "old operator"
  | Ttrue -> [%expr true]
  | Tfalse -> [%expr false]

let term ~driver fail t =
  try
    Ok
      [%expr
        try [%e unsafe_term ~driver t]
        with e ->
          [%e fail (evar "e")];
          true]
  with W.Error t -> Error t

let conditions ~driver ~term_printer fail_violated fail_nonexec terms =
  List.map
    (fun t ->
      let txt = term_printer t in
      let loc = Option.value ~default:Location.none t.Tterm.t_loc in
      let translation =
        term ~driver (fail_nonexec txt) t
        |> Result.map (fun t ->
               [%expr if not [%e t] then [%e fail_violated txt]])
      in
      { txt; loc; translation })
    terms

let with_pres ~driver ~term_printer pres (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let preconditions = conditions ~driver ~term_printer violated nonexec pres in
  { value with preconditions }

let with_posts ~driver ~term_printer posts (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Post ~term ~register_name in
  let nonexec term exn = F.spec_failure `Post ~term ~exn ~register_name in
  let postconditions =
    conditions ~driver ~term_printer violated nonexec posts
  in
  { value with postconditions }

let rec xpost_pattern ~driver exn = function
  | Tterm.Papp (ls, []) when Tterm.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Tterm.Papp (ls, _l) when not (Tterm.is_fs_tuple ls) -> assert false
  | Tterm.Por (p1, p2) ->
      ppat_or
        (xpost_pattern ~driver exn p1.p_node)
        (xpost_pattern ~driver exn p2.p_node)
  | Tterm.Pas (p, s) ->
      ppat_alias
        (xpost_pattern ~driver exn p.p_node)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))
  | pn -> ppat_construct (lident exn) (Some (pattern pn))

module M = Map.Make (struct
  type t = Ttypes.xsymbol

  let compare = compare
end)

let assert_false_case =
  case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]

let with_xposts ~driver ~term_printer xposts (value : value) =
  let register_name = evar value.register_name in
  let xpost (exn, ptlist) =
    let name = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
          let s = term_printer t in
          let nonexec exn = F.spec_failure `XPost ~term:s ~exn ~register_name in
          term ~driver nonexec t
          |> Result.map (fun t ->
                 case ~guard:None
                   ~lhs:(xpost_pattern ~driver name p.Tterm.p_node)
                   ~rhs:
                     [%expr
                       if not [%e t] then
                         [%e F.violated `XPost ~term:s ~register_name]]))
        (* XXX ptlist must be rev because the cases are given in the
           reverse order by gospel *)
        (List.rev ptlist)
    in
    if List.exists Result.is_error cases then
      List.filter_map (function Ok _ -> None | Error x -> Some x) cases
      |> Result.error
    else List.map Result.get_ok cases @ [ assert_false_case ] |> Result.ok
  in
  let xpostconditions =
    List.map
      (fun xp ->
        let exn = (fst xp).Ttypes.xs_ident.id_str in
        let translation = xpost xp in
        { exn; translation })
      xposts
  in
  { value with xpostconditions }

let axiom_definition ~driver ~register_name t =
  let register_name = evar register_name in
  let fail_violated = F.violated_axiom ~register_name in
  let fail_nonexec exn = F.axiom_failure ~exn ~register_name in
  let txt = Fmt.str "%a" Tterm.print_term t in
  let loc = Option.value ~default:Location.none t.t_loc in
  let translation =
    term ~driver fail_nonexec t
    |> Result.map (fun check ->
           [%expr
             if not [%e check] then [%e fail_violated];
             [%e F.report ~register_name]])
  in
  { txt; loc; translation }

let returned_pattern rets =
  let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
  let pvars, evars =
    List.filter_map
      (function
        | Tast.Lunit -> Some (punit, eunit)
        | Tast.Lnone x ->
            let s = to_string x in
            Some (pvar s, evar s)
        | Tast.Lghost _ -> None
        | Tast.Loptional _ | Tast.Lnamed _ -> assert false)
      rets
    |> List.split
  in
  (ppat_tuple pvars, pexp_tuple evars)

let mk_setup loc fun_name =
  let loc_name = gen_symbol ~prefix:"__loc" () in
  let let_loc next =
    [%expr
      let [%p pvar loc_name] = [%e elocation loc] in
      [%e next]]
  in
  let register_name = gen_symbol ~prefix:"__acc" () in
  let let_acc next =
    [%expr
      let [%p pvar register_name] =
        Ortac_runtime.Errors.create [%e evar loc_name] [%e estring fun_name]
      in
      [%e next]]
  in
  ((fun next -> let_loc @@ let_acc @@ next), register_name)

(* let mk_call ~driver ~register_name ~term_printer ret_pat loc fun_name xpost *)
(*     eargs = *)
(*   let call = pexp_apply (evar fun_name) eargs in *)
(*   let check_raises = *)
(*     xpost_guard ~driver ~register_name ~term_printer xpost call *)
(*   in *)
(*   fun next -> *)
(*     [%expr *)
(*       let [%p ret_pat] = [%e check_raises] in *)
(*       [%e next]] *)

let mk_function_def ~driver t =
  try Some (unsafe_term ~driver t)
  with W.Error t ->
    W.register t;
    None

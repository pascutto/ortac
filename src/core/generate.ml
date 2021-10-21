module W = Warnings
open Ppxlib
open Builder
open Translated
module F = Failure
module T = Translation
module M = Map.Make (String)

let setup name loc register_name next =
  [%expr
    let [%p pvar register_name] =
      Ortac_runtime.Errors.create [%e elocation loc] [%e estring name]
    in
    [%e next]]

let group_xpost (value : Translated.value) =
  let register_name = evar value.register_name in
  let default_cases =
    [
      case ~guard:None
        ~lhs:[%pat? (Stack_overflow | Out_of_memory) as e]
        ~rhs:[%expr raise e];
      case ~guard:None
        ~lhs:[%pat? e]
        ~rhs:
          [%expr
            [%e F.unexpected_exn ~allowed_exn:[] ~exn:(evar "e") ~register_name];
            [%e F.report ~register_name];
            raise e];
    ]
  in
  let tbl = Hashtbl.create 0 in
  let rec aux keys = function
    | [] -> keys
    | { exn; args; translation = Ok translation } :: t ->
        Hashtbl.add tbl exn translation;
        aux (M.add exn args keys) t
    | _ :: t -> aux keys t
  in
  aux M.empty value.xpostconditions |> fun s ->
  M.fold
    (fun exn args acc ->
      let e = gen_symbol ~prefix:"__e_" () in
      let lhs =
        ppat_alias
          (ppat_construct (lident exn)
             (if args = 0 then None else Some ppat_any))
          (noloc e)
      in
      let matches =
        Hashtbl.find_all tbl exn |> List.map (pexp_match (evar e)) |> esequence
      in
      let rhs =
        esequence
          [ matches; F.report ~register_name; eapply (evar "raise") [ evar e ] ]
      in
      case ~guard:None ~lhs ~rhs :: acc)
    s default_cases

let sequence_conditions terms next =
  List.fold_left
    (fun acc (t : Translated.term) ->
      match t.translation with Error _ -> acc | Ok c -> pexp_sequence c acc)
    next terms

let invariants ignore_consumes =
  List.concat_map (fun (a : Translated.ocaml_var) ->
      if ignore_consumes && a.consumed then [] else a.type_.invariants)

let args f = List.map (fun a -> (a.label, f a.name))

let rets (returns : ocaml_var list) =
  match returns with
  | [] -> (eunit, punit)
  | [ x ] -> (evar x.name, pvar x.name)
  | ret ->
      List.fold_right
        (fun (x : ocaml_var) (e, p) -> (evar x.name :: e, pvar x.name :: p))
        ret ([], [])
      |> fun (e, p) -> (pexp_tuple e, ppat_tuple p)

let value ~driver (value : Translated.value) =
  let register_name = evar value.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let eargs = args evar value.arguments in
  let pargs = args pvar value.arguments in
  let eret, pret = rets value.returns in
  let call_name = Fmt.str "%s.%s" (Drv.module_name driver) value.name in
  let call = pexp_apply (evar call_name) eargs in
  let try_call = pexp_try call (group_xpost value) in
  let body =
    setup value.name value.loc value.register_name
    @@ sequence_conditions value.preconditions
    @@ sequence_conditions (invariants false value.arguments)
    @@ report
    @@ pexp_let Nonrecursive [ value_binding ~pat:pret ~expr:try_call ]
    @@ sequence_conditions value.postconditions
    @@ sequence_conditions (invariants true value.arguments)
    @@ sequence_conditions (invariants false value.returns)
    @@ report
    @@ eret
  in
  [ [%stri let [%p pvar value.name] = [%e efun pargs body]] ]

let function_ ~driver:_ (f : Translated.function_) =
  match f.definition with
  | Some { translation = Ok def; _ } ->
      let pat = pvar f.name in
      let pargs = args pvar f.arguments in
      let expr = efun pargs def in
      let rec_flag = if f.rec_ then Recursive else Nonrecursive in
      [ pstr_value rec_flag [ value_binding ~pat ~expr ] ]
  | _ -> []

let structure driver : structure =
  (pmod_ident (lident (Drv.module_name driver)) |> include_infos |> pstr_include)
  :: (Drv.map_translation driver ~f:(function
        | Translated.Value v -> value ~driver v
        | Translated.Function f -> function_ ~driver f
        | Translated.Predicate f -> function_ ~driver f
        | _ -> [])
     |> List.flatten)

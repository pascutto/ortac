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
      match t.translation with
      | Error e ->
          W.register e;
          acc
      | Ok c -> pexp_sequence c acc)
    next terms

let value ~driver (value : Translated.value) =
  let register_name = evar value.register_name in
  let report = pexp_sequence (F.report ~register_name) in
  let eargs, pargs =
    List.map
      (fun (var : ocaml_var) ->
        ((var.label, evar var.name), (var.label, pvar var.name)))
      value.arguments
    |> List.split
  in
  let eret, pret =
    match value.returns with
    | [] -> (eunit, punit)
    | [ x ] -> (evar x.name, pvar x.name)
    | ret ->
        List.map (fun (x : ocaml_var) -> (evar x.name, pvar x.name)) ret
        |> List.split
        |> fun (e, p) -> (pexp_tuple e, ppat_tuple p)
  in
  let setup = setup value.name value.loc value.register_name in
  let invariants_pre =
    sequence_conditions
      (List.concat_map
         (fun (a : Translated.ocaml_var) -> a.type_.invariants)
         value.arguments)
  in
  let pres = sequence_conditions value.preconditions in
  let call_name = Fmt.str "%s.%s" (Drv.module_name driver) value.name in
  let call = pexp_apply (evar call_name) eargs in
  let try_call = pexp_try call (group_xpost value) in
  let posts = sequence_conditions value.postconditions in
  let invariants_post =
    sequence_conditions
      (List.concat_map
         (fun (a : Translated.ocaml_var) ->
           if a.consumed then [] else a.type_.invariants)
         value.arguments)
  in
  let ret_invariants =
    sequence_conditions
      (List.concat_map
         (fun (a : Translated.ocaml_var) -> a.type_.invariants)
         value.returns)
  in
  let body =
    setup
    @@ pres
    @@ invariants_pre
    @@ report
    @@ pexp_let Nonrecursive [ value_binding ~pat:pret ~expr:try_call ]
    @@ posts
    @@ invariants_post (* XXX TODO: handle modified *)
    @@ ret_invariants
    @@ report
    @@ eret
  in
  [ [%stri let [%p pvar value.name] = [%e efun pargs body]] ]

let function_ ~driver:_ (func : Translated.function_) =
  match func.definition with
  | None -> []
  | Some term -> (
      match term.translation with
      | Error _ -> []
      | Ok def ->
          let pat = pvar func.name in
          let rec f = function
            | [] -> def
            | arg :: args -> pexp_fun arg.label None (pvar arg.name) (f args)
          in
          let rec_flag = if func.rec_ then Recursive else Nonrecursive in
          let expr = f func.arguments in
          [ pstr_value rec_flag [ value_binding ~pat ~expr ] ])

let structure driver : structure =
  (pmod_ident (lident (Drv.module_name driver)) |> include_infos |> pstr_include)
  :: (Drv.map_translation driver ~f:(function
        | Translated.Value v -> value ~driver v
        | Translated.Function f -> function_ ~driver f
        | _ -> [])
     |> List.flatten
     |> List.rev)

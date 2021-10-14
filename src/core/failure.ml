open Ppxlib
open Builder

let eterm t = estring t

let term_kind kind =
  (match kind with
  | `Pre -> "Pre"
  | `Post -> "Post"
  | `XPost -> "XPost"
  | `Invariant -> "Invariant")
  |> lident
  |> fun c -> pexp_construct c None

let register ~register_name e =
  [%expr [%e e] |> Ortac_runtime.Errors.register [%e register_name]]

let violated kind ~term ~register_name =
  [%expr
    Ortac_runtime.Violated_condition
      { term = [%e eterm term]; term_kind = [%e term_kind kind] }]
  |> register ~register_name

let violated_axiom ~register_name =
  register ~register_name [%expr Ortac_runtime.Violated_axiom]

let axiom_failure ~exn ~register_name =
  [%expr Ortac_runtime.Axiom_failure { exn = [%e exn] }]
  |> register ~register_name

let spec_failure kind ~term ~exn ~register_name =
  [%expr
    Ortac_runtime.Specification_failure
      {
        term = [%e eterm term];
        term_kind = [%e term_kind kind];
        exn = [%e exn];
      }]
  |> register ~register_name

let unexpected_exn ~allowed_exn ~exn ~register_name =
  let allowed_exn = List.map estring allowed_exn |> elist in
  [%expr
    Ortac_runtime.Unexpected_exception
      { allowed_exn = [%e allowed_exn]; exn = [%e exn] }]
  |> register ~register_name

let uncaught_checks ~term ~register_name =
  [%expr Ortac_runtime.Uncaught_checks { term = [%e eterm term] }]
  |> register ~register_name

let unexpected_checks ~terms ~register_name =
  let terms = List.map eterm terms |> elist in
  [%expr Ortac_runtime.Unexpected_checks { terms = [%e terms] }]
  |> register ~register_name

let report ~register_name =
  [%expr Ortac_runtime.Errors.report [%e register_name]]

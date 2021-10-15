open Fmt

type location = { start : Lexing.position; stop : Lexing.position }
type term_kind = Pre | Post | XPost

type error =
  | Violated_axiom
  | Axiom_failure of { exn : exn }
  | Violated_condition of { term : string; term_kind : term_kind }
  | Specification_failure of { term : string; term_kind : term_kind; exn : exn }
  | Unexpected_exception of { allowed_exn : string list; exn : exn }
  | Uncaught_checks of { term : string }
  | Unexpected_checks of { terms : string list }

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l
let quoted pp ppf = pf ppf "`%a'" pp

let pp_term_kind =
  using
    (function
      | Pre -> "pre-condition"
      | Post -> "post-condition"
      | XPost -> "exceptional post-condition")
    (styled `Yellow string)

let pp_term = quoted (styled `Bold string)
let pp_terms = list ~sep:(any "@\n") pp_term

let pp_loc =
  let unstyled ppf loc =
    pf ppf "File \"%s\", line %d, characters %d-%d:" loc.start.pos_fname
      loc.start.pos_lnum
      (loc.start.pos_cnum - loc.start.pos_bol)
      (loc.stop.pos_cnum - loc.start.pos_bol)
  in
  styled_list [ `Underline; `Bold ] unstyled

let pp_fun_name = quoted (styled `Blue string)
let pp_quoted_exn = quoted (styled `Bold string)
let pp_exn = using Printexc.to_string pp_quoted_exn
let pp_allowed_exn = list ~sep:comma pp_quoted_exn

let pp_error ppf = function
  | Violated_axiom -> pf ppf "the axiom was %a." (styled `Red string) "violated"
  | Axiom_failure { exn } ->
      pf ppf "the evaluation of the axiom %a:@\n  @[%a@]"
        (styled `Red string)
        "raised an exception" pp_exn exn
  | Violated_condition { term; term_kind } ->
      pf ppf "the %a@\n  @[%a@]@\nwas %a." pp_term_kind term_kind pp_term term
        (styled `Red string)
        "violated"
  | Specification_failure { term; term_kind; exn } ->
      pf ppf "the evaluation of the %a@\n  @[%a@]@\n%a:@\n  @[%a@]" pp_term_kind
        term_kind pp_term term
        (styled `Red string)
        "raised an exception" pp_exn exn
  | Unexpected_exception { allowed_exn; exn } ->
      pf ppf
        "it raised an %a:@\n\
        \  @[%a@]@\n\
         only the following exceptions were declared:@\n\
        \  @[%a@]"
        (styled `Red string)
        "unexpected exception" pp_exn exn pp_allowed_exn allowed_exn
  | Uncaught_checks { term } ->
      pf ppf
        "a %a in@\n\
        \  @[%a@]@\n\
         was not detected.@\n\
         Function should have raised %a."
        (styled `Red string)
        "`checks' precondition violation" pp_term term pp_quoted_exn
        "Invalid_argument"
  | Unexpected_checks { terms } ->
      pf ppf
        "it %a@\n\
        \   @[%a@]\n\
         but none of the declared `checks' preconditions@\n\
        \  @[%a@]\n\
         were violated."
        (styled `Red string)
        "raised exception" pp_quoted_exn "Invalid_argument" pp_terms terms

type error_report = {
  loc : location;
  fun_name : string;
  mutable errors : error list;
}

let pp_error_report ppf { loc; fun_name; errors } =
  let pp_bullet pp ppf = pf ppf "- @[%a@]" pp in
  pf ppf "%a@\n%a in function %a@\n  @[%a@]" pp_loc loc
    (styled_list [ `Bold; `Red ] string)
    "Runtime error" pp_fun_name fun_name
    (list ~sep:(any "@\n") (pp_bullet pp_error))
    errors

exception Error of error_report

module Errors = struct
  type t = error_report

  let create loc fun_name = { loc; fun_name; errors = [] }
  let register t e = t.errors <- e :: t.errors

  let report t =
    match t.errors with
    | [] -> ()
    | _ ->
        pf stderr "%a@." pp_error_report t;
        raise (Error t)
end

module Z = struct
  include Z

  let pow x n =
    try pow x (to_int n) with Overflow -> invalid_arg "Exponent too big"

  let rec forall start stop p =
    start > stop || (p start && forall (succ start) stop p)

  let rec exists start stop p =
    start <= stop && (p start || exists (succ start) stop p)
end

module Array = struct
  let make z =
    if Z.(z > of_int Sys.max_array_length) then
      raise (Invalid_argument "Array length too big")
    else Array.make (Z.to_int z)

  let get arr z =
    if Z.(z < zero || z >= of_int (Array.length arr)) then
      raise (Invalid_argument "Out of array bounds")
    else Array.unsafe_get arr (Z.to_int z)

  let length arr = Array.length arr |> Z.of_int
  let for_all = Array.for_all
end

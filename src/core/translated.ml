module W = Warnings
open Ppxlib
open Gospel

type type_ = {
  name : string;
  loc : Location.t;
  mutable_ : bool;
  invariants : expression list;
  equality : expression;
  comparison : expression;
  copy : expression;
}

type ocaml_var = {
  name : string;
  type_ : Ttypes.ty;
  invariants : expression list;
}

type term = {
  txt : string;
  loc : Location.t;
  translation : (expression, W.t) result;
}

type xpost = { exn : string; translation : (cases, W.t list) result }

type value = {
  name : string;
  loc : Location.t;
  arguments : ocaml_var list;
  returns : ocaml_var list;
  register_name : string;
  pure : bool;
  preconditions : term list;
  postconditions : term list;
  xpostconditions : xpost list;
}

type constant = {
  name : string;
  loc : Location.t;
  type_ : Ttypes.ty;
  register_name : string;
  checks : term list;
  invariants : expression list;
}

let constant ~name ~loc ~type_ ~register_name =
  { name; loc; type_; register_name; checks = []; invariants = [] }

let value ~name ~loc ~arguments ~returns ~register_name ~pure =
  {
    name;
    loc;
    arguments;
    returns;
    register_name;
    pure;
    preconditions = [];
    postconditions = [];
    xpostconditions = [];
  }

type axiom = {
  name : string;
  loc : Location.t;
  register_name : string;
  definition : term;
}

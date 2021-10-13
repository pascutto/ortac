module W = Warnings
open Ppxlib
open Gospel

type term = {
  txt : string;
  loc : Location.t;
  translation : (expression, W.t) result;
}

type ocaml_var = { name : string; type_ : Ttypes.ty; invariants : term list }

type type_ = {
  name : string;
  loc : Location.t;
  mutable_ : bool;
  ghost : bool;
  models : (string * bool) list;
  invariants : term list;
  equality : (expression, W.t) result;
  comparison : (expression, W.t) result;
  copy : (expression, W.t) result;
}

let type_ ~name ~loc ~mutable_ ~ghost =
  {
    name;
    loc;
    mutable_;
    ghost;
    models = [];
    invariants = [];
    equality = Error (W.Unsupported "equality", loc);
    comparison = Error (W.Unsupported "comparison", loc);
    copy = Error (W.Unsupported "copy", loc);
  }

type xpost = { exn : string; translation : (cases, W.t list) result }

type value = {
  name : string;
  loc : Location.t;
  arguments : ocaml_var list;
  returns : ocaml_var list;
  register_name : string;
  ghost : bool;
  pure : bool;
  preconditions : term list;
  postconditions : term list;
  xpostconditions : xpost list;
}

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

type constant = {
  name : string;
  loc : Location.t;
  type_ : Ttypes.ty;
  register_name : string;
  ghost : bool;
  checks : term list;
  invariants : expression list;
}

let constant ~name ~loc ~type_ ~register_name ~ghost =
  { name; loc; type_; register_name; ghost; checks = []; invariants = [] }

type axiom = {
  name : string;
  loc : Location.t;
  register_name : string;
  definition : term;
}

type function_ = { name : string; loc : Location.t; definition : term option }

type structure_item =
  | Type of type_
  | Value of value
  | Constant of constant
  | Function of function_
  | Predicate of function_
  | Axiom of axiom

module InjSide = InjSide;

module Typ = Typ;
[@deriving sexp]
type typ = Typ.t;
module Expr = Expr;
[@deriving sexp]
type expr = Expr.t;
[@deriving sexp]
type case = Expr.case;
[@deriving sexp]
type rule = Expr.rule;

module Pat = Pat;
[@deriving sexp]
type pat = Pat.t;

module Syn = Syn;
let syn: (Ident.Map.t(Typ.t), Delta.t, Expr.t) => Syn.syn_result;

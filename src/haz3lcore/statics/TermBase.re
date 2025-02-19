open Util;

let continue = x => x;
let stop = (_, x) => x;
[@deriving (show({with_path: false}), sexp, yojson)]
type deferral_position_t =
  | InAp
  | OutsideAp;

/*
   This megafile contains the definitions of the expression data types in
   Hazel. They are all in one file because they are mutually recursive, and
   OCaml doesn't let us have mutually recursive files. Any definition that
   is not mutually recursive across the whole data structure should be
   defined in Any.re, Exp.re, Typ.re, Pat.re, TPat.re, etc...

   Each module has:

   - A type definition for the term

   - A map_term function that allows you to apply a function to every term in
     the data structure with the following type:

     map_term:
     (
       ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
       ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
       ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
       ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
       ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
       ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
       t
     ) =>
     t;

     Each argument to `map_term` specifies what should happen at each node in the
     data structure. Each function takes two arguments: a `continue` function that
     allows the map to continue on all the children nodes, and the current node
     itself. If you don't explicitly call the `continue` function, the map will
     not traverse the children nodes. If you don't provide a function for a
     specific kind of node, the map will simply continue at that node without
     any additional action.

   - A fast_equal function that compares two terms for equality, it performs
     structural equality except for the case of closures, where it just compares
     the id of the closure.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type any_t =
  | Exp(exp_t)
  | Pat(pat_t)
  | Typ(typ_t)
  | TPat(tpat_t)
  | Rul(rul_t)
  | Any(unit)
and exp_term =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t))
  | DynamicErrorHole(exp_t, InvalidOperationError.t)
  | FailedCast(exp_t, typ_t, typ_t)
  | Deferral(deferral_position_t)
  | Undefined
  | Bool(bool)
  | Int(int)
  | Float(float)
  | String(string)
  | ListLit(list(exp_t))
  | Constructor(string, typ_t) // Typ.t field is only meaningful in dynamic expressions
  | Fun(pat_t, exp_t, option(typ_t), option(Var.t)) // typ_t field is only used to display types in results
  | TypFun(tpat_t, exp_t, option(Var.t))
  | Tuple(list(exp_t))
  | Var(Var.t)
  | Let(pat_t, exp_t, exp_t)
  | FixF(pat_t, exp_t, option(closure_environment_t))
  | TyAlias(tpat_t, typ_t, exp_t)
  | Ap(Operators.ap_direction, exp_t, exp_t)
  | TypAp(exp_t, typ_t)
  | DeferredAp(exp_t, list(exp_t))
  | If(exp_t, exp_t, exp_t)
  | Seq(exp_t, exp_t)
  | Test(exp_t)
  | Filter(stepper_filter_kind_t, exp_t)
  | Closure([@show.opaque] closure_environment_t, exp_t)
  | Parens(exp_t) // (
  | Cons(exp_t, exp_t)
  | ListConcat(exp_t, exp_t)
  | UnOp(Operators.op_un, exp_t)
  | BinOp(Operators.op_bin, exp_t, exp_t)
  | BuiltinFun(string)
  | Match(exp_t, list((pat_t, exp_t)))
  /* INVARIANT: in dynamic expressions, casts must be between
     two consistent types. Both types should be normalized in
     dynamics for the cast calculus to work right. */
  | Cast(exp_t, typ_t, typ_t)
and exp_t = IdTagged.t(exp_term)
and pat_term =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t))
  | Wild
  | Int(int)
  | Float(float)
  | Bool(bool)
  | String(string)
  | ListLit(list(pat_t))
  | Constructor(string, typ_t) // Typ.t field is only meaningful in dynamic patterns
  | Cons(pat_t, pat_t)
  | Var(Var.t)
  | Tuple(list(pat_t))
  | Parens(pat_t)
  | Ap(pat_t, pat_t)
  | Cast(pat_t, typ_t, typ_t)
and pat_t = IdTagged.t(pat_term)
and typ_term =
  | Unknown(type_provenance)
  | Int
  | Float
  | Bool
  | String
  | Var(string)
  | List(typ_t)
  | Arrow(typ_t, typ_t)
  | Sum(ConstructorMap.t(typ_t))
  | Prod(list(typ_t))
  | Parens(typ_t)
  | Ap(typ_t, typ_t)
  | Rec(tpat_t, typ_t)
  | Forall(tpat_t, typ_t)
and typ_t = IdTagged.t(typ_term)
and tpat_term =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t))
  | Var(string)
and tpat_t = IdTagged.t(tpat_term)
and rul_term =
  | Invalid(string)
  | Hole(list(any_t))
  | Rules(exp_t, list((pat_t, exp_t)))
and rul_t = IdTagged.t(rul_term)
and environment_t = VarBstMap.Ordered.t_(exp_t)
and closure_environment_t = (Id.t, environment_t)
and stepper_filter_kind_t =
  | Filter(filter)
  | Residue(int, FilterAction.t)
and type_hole =
  | Invalid(string)
  | EmptyHole
  | MultiHole(list(any_t))
and type_provenance =
  | SynSwitch
  | Hole(type_hole)
  | Internal
and filter = {
  pat: exp_t,
  act: FilterAction.t,
};

module rec Any: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = any_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let rec_call = y =>
      switch (y) {
      | Exp(x) =>
        Exp(Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Pat(x) =>
        Pat(Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Typ(x) =>
        Typ(Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | TPat(x) =>
        TPat(
          TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x),
        )
      | Rul(x) =>
        Rul(Rul.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any, x))
      | Any () => Any()
      };
    x |> f_any(rec_call);
  };

  let fast_equal = (x, y) =>
    switch (x, y) {
    | (Exp(x), Exp(y)) => Exp.fast_equal(x, y)
    | (Pat(x), Pat(y)) => Pat.fast_equal(x, y)
    | (Typ(x), Typ(y)) => Typ.fast_equal(x, y)
    | (TPat(x), TPat(y)) => TPat.fast_equal(x, y)
    | (Rul(x), Rul(y)) => Rul.fast_equal(x, y)
    | (Any (), Any ()) => true
    | (Exp(_), _)
    | (Pat(_), _)
    | (Typ(_), _)
    | (TPat(_), _)
    | (Rul(_), _)
    | (Any (), _) => false
    };
}
and Exp: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;
  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = exp_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = exp_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type deferral_position = deferral_position_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let flt_map_term =
      StepperFilterKind.map_term(
        ~f_exp,
        ~f_pat,
        ~f_typ,
        ~f_tpat,
        ~f_rul,
        ~f_any,
      );
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Deferral(_)
        | Var(_)
        | Undefined => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | DynamicErrorHole(e, err) => DynamicErrorHole(exp_map_term(e), err)
        | FailedCast(e, t1, t2) =>
          FailedCast(exp_map_term(e), typ_map_term(t1), typ_map_term(t2))
        | ListLit(ts) => ListLit(List.map(exp_map_term, ts))
        | Fun(p, e, t, f) =>
          Fun(
            pat_map_term(p),
            exp_map_term(e),
            Option.map(typ_map_term, t),
            f,
          )
        | TypFun(tp, e, f) => TypFun(tpat_map_term(tp), exp_map_term(e), f)
        | Tuple(xs) => Tuple(List.map(exp_map_term, xs))
        | Let(p, e1, e2) =>
          Let(pat_map_term(p), exp_map_term(e1), exp_map_term(e2))
        | FixF(p, e, env) => FixF(pat_map_term(p), exp_map_term(e), env)
        | TyAlias(tp, t, e) =>
          TyAlias(tpat_map_term(tp), typ_map_term(t), exp_map_term(e))
        | Ap(op, e1, e2) => Ap(op, exp_map_term(e1), exp_map_term(e2))
        | TypAp(e, t) => TypAp(exp_map_term(e), typ_map_term(t))
        | DeferredAp(e, es) =>
          DeferredAp(exp_map_term(e), List.map(exp_map_term, es))
        | If(e1, e2, e3) =>
          If(exp_map_term(e1), exp_map_term(e2), exp_map_term(e3))
        | Seq(e1, e2) => Seq(exp_map_term(e1), exp_map_term(e2))
        | Test(e) => Test(exp_map_term(e))
        | Filter(f, e) => Filter(flt_map_term(f), exp_map_term(e))
        | Closure(env, e) => Closure(env, exp_map_term(e))
        | Parens(e) => Parens(exp_map_term(e))
        | Cons(e1, e2) => Cons(exp_map_term(e1), exp_map_term(e2))
        | ListConcat(e1, e2) =>
          ListConcat(exp_map_term(e1), exp_map_term(e2))
        | UnOp(op, e) => UnOp(op, exp_map_term(e))
        | BinOp(op, e1, e2) =>
          BinOp(op, exp_map_term(e1), exp_map_term(e2))
        | BuiltinFun(str) => BuiltinFun(str)
        | Match(e, rls) =>
          Match(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        | Cast(e, t1, t2) =>
          Cast(exp_map_term(e), typ_map_term(t1), typ_map_term(t2))
        },
    };
    x |> f_exp(rec_call);
  };

  let rec fast_equal = (e1, e2) =>
    switch (e1 |> IdTagged.term_of, e2 |> IdTagged.term_of) {
    | (DynamicErrorHole(x, _), _)
    | (Parens(x), _) => fast_equal(x, e2)
    | (_, DynamicErrorHole(x, _))
    | (_, Parens(x)) => fast_equal(e1, x)
    | (EmptyHole, EmptyHole) => true
    | (Undefined, Undefined) => true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) when List.length(xs) == List.length(ys) =>
      List.equal(Any.fast_equal, xs, ys)
    | (FailedCast(e1, t1, t2), FailedCast(e2, t3, t4)) =>
      Exp.fast_equal(e1, e2)
      && Typ.fast_equal(t1, t3)
      && Typ.fast_equal(t2, t4)
    | (Deferral(d1), Deferral(d2)) => d1 == d2
    | (Bool(b1), Bool(b2)) => b1 == b2
    | (Int(i1), Int(i2)) => i1 == i2
    | (Float(f1), Float(f2)) => f1 == f2
    | (String(s1), String(s2)) => s1 == s2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Constructor(c1, ty1), Constructor(c2, ty2)) =>
      c1 == c2 && Typ.fast_equal(ty1, ty2)
    | (Fun(p1, e1, t1, _), Fun(p2, e2, t2, _)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(Typ.fast_equal, t1, t2)
    | (TypFun(tp1, e1, _), TypFun(tp2, e2, _)) =>
      TPat.fast_equal(tp1, tp2) && fast_equal(e1, e2)
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Var(v1), Var(v2)) => v1 == v2
    | (Let(p1, e1, e2), Let(p2, e3, e4)) =>
      Pat.fast_equal(p1, p2) && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (FixF(p1, e1, c1), FixF(p2, e2, c2)) =>
      Pat.fast_equal(p1, p2)
      && fast_equal(e1, e2)
      && Option.equal(ClosureEnvironment.id_equal, c1, c2)
    | (TyAlias(tp1, t1, e1), TyAlias(tp2, t2, e2)) =>
      TPat.fast_equal(tp1, tp2)
      && Typ.fast_equal(t1, t2)
      && fast_equal(e1, e2)
    | (Ap(d1, e1, e2), Ap(d2, e3, e4)) =>
      d1 == d2 && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (TypAp(e1, t1), TypAp(e2, t2)) =>
      fast_equal(e1, e2) && Typ.fast_equal(t1, t2)
    | (DeferredAp(e1, es1), DeferredAp(e2, es2)) =>
      List.length(es1) == List.length(es2)
      && fast_equal(e1, e2)
      && List.equal(fast_equal, es1, es2)
    | (If(e1, e2, e3), If(e4, e5, e6)) =>
      fast_equal(e1, e4) && fast_equal(e2, e5) && fast_equal(e3, e6)
    | (Seq(e1, e2), Seq(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (Test(e1), Test(e2)) => fast_equal(e1, e2)
    | (Filter(f1, e1), Filter(f2, e2)) =>
      StepperFilterKind.fast_equal(f1, f2) && fast_equal(e1, e2)
    | (Closure(c1, e1), Closure(c2, e2)) =>
      ClosureEnvironment.id_equal(c1, c2) && fast_equal(e1, e2)
    | (Cons(e1, e2), Cons(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (ListConcat(e1, e2), ListConcat(e3, e4)) =>
      fast_equal(e1, e3) && fast_equal(e2, e4)
    | (UnOp(o1, e1), UnOp(o2, e2)) => o1 == o2 && fast_equal(e1, e2)
    | (BinOp(o1, e1, e2), BinOp(o2, e3, e4)) =>
      o1 == o2 && fast_equal(e1, e3) && fast_equal(e2, e4)
    | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
    | (Match(e1, rls1), Match(e2, rls2)) =>
      fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Cast(e1, t1, t2), Cast(e2, t3, t4)) =>
      fast_equal(e1, e2) && Typ.fast_equal(t1, t3) && Typ.fast_equal(t2, t4)
    | (Invalid(_), _)
    | (FailedCast(_), _)
    | (Deferral(_), _)
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (String(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Fun(_), _)
    | (TypFun(_), _)
    | (Tuple(_), _)
    | (Var(_), _)
    | (Let(_), _)
    | (FixF(_), _)
    | (TyAlias(_), _)
    | (Ap(_), _)
    | (TypAp(_), _)
    | (DeferredAp(_), _)
    | (If(_), _)
    | (Seq(_), _)
    | (Test(_), _)
    | (Filter(_), _)
    | (Closure(_), _)
    | (Cons(_), _)
    | (ListConcat(_), _)
    | (UnOp(_), _)
    | (BinOp(_), _)
    | (BuiltinFun(_), _)
    | (Match(_), _)
    | (Cast(_), _)
    | (MultiHole(_), _)
    | (EmptyHole, _)
    | (Undefined, _) => false
    };
}
and Pat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term; // The second Typ.t field is only meaningful in dynamic patterns
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = pat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Wild
        | Bool(_)
        | Int(_)
        | Float(_)
        | Constructor(_)
        | String(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        | ListLit(ts) => ListLit(List.map(pat_map_term, ts))
        | Ap(e1, e2) => Ap(pat_map_term(e1), pat_map_term(e2))
        | Cons(e1, e2) => Cons(pat_map_term(e1), pat_map_term(e2))
        | Tuple(xs) => Tuple(List.map(pat_map_term, xs))
        | Parens(e) => Parens(pat_map_term(e))
        | Cast(e, t1, t2) =>
          Cast(pat_map_term(e), typ_map_term(t1), typ_map_term(t2))
        },
    };
    x |> f_pat(rec_call);
  };

  let rec fast_equal = (p1: t, p2: t) =>
    switch (p1 |> IdTagged.term_of, p2 |> IdTagged.term_of) {
    | (Parens(x), _) => fast_equal(x, p2)
    | (_, Parens(x)) => fast_equal(p1, x)
    | (EmptyHole, EmptyHole) => true
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Wild, Wild) => true
    | (Bool(b1), Bool(b2)) => b1 == b2
    | (Int(i1), Int(i2)) => i1 == i2
    | (Float(f1), Float(f2)) => f1 == f2
    | (String(s1), String(s2)) => s1 == s2
    | (Constructor(c1, t1), Constructor(c2, t2)) =>
      c1 == c2 && Typ.fast_equal(t1, t2)
    | (Var(v1), Var(v2)) => v1 == v2
    | (ListLit(xs), ListLit(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Cons(x1, y1), Cons(x2, y2)) =>
      fast_equal(x1, x2) && fast_equal(y1, y2)
    | (Tuple(xs), Tuple(ys)) =>
      List.length(xs) == List.length(ys) && List.equal(fast_equal, xs, ys)
    | (Ap(x1, y1), Ap(x2, y2)) => fast_equal(x1, x2) && fast_equal(y1, y2)
    | (Cast(x1, t1, t2), Cast(x2, u1, u2)) =>
      fast_equal(x1, x2) && Typ.fast_equal(t1, u1) && Typ.fast_equal(t2, u2)
    | (EmptyHole, _)
    | (MultiHole(_), _)
    | (Invalid(_), _)
    | (Wild, _)
    | (Bool(_), _)
    | (Int(_), _)
    | (Float(_), _)
    | (String(_), _)
    | (ListLit(_), _)
    | (Constructor(_), _)
    | (Cons(_), _)
    | (Var(_), _)
    | (Tuple(_), _)
    | (Ap(_), _)
    | (Cast(_), _) => false
    };
}
and Typ: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let subst: (t, TPat.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = typ_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = typ_t;

  type sum_map = ConstructorMap.t(t);

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let typ_map_term =
      Typ.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let tpat_map_term =
      TPat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Unknown(Hole(EmptyHole))
        | Unknown(Hole(Invalid(_)))
        | Unknown(SynSwitch)
        | Unknown(Internal)
        | Bool
        | Int
        | Float
        | String
        | Var(_) => term
        | List(t) => List(typ_map_term(t))
        | Unknown(Hole(MultiHole(things))) =>
          Unknown(Hole(MultiHole(List.map(any_map_term, things))))
        | Ap(e1, e2) => Ap(typ_map_term(e1), typ_map_term(e2))
        | Prod(xs) => Prod(List.map(typ_map_term, xs))
        | Parens(e) => Parens(typ_map_term(e))
        | Arrow(t1, t2) => Arrow(typ_map_term(t1), typ_map_term(t2))
        | Sum(variants) =>
          Sum(
            List.map(
              fun
              | ConstructorMap.Variant(c, ids, t) =>
                ConstructorMap.Variant(c, ids, Option.map(typ_map_term, t))
              | ConstructorMap.BadEntry(t) =>
                ConstructorMap.BadEntry(typ_map_term(t)),
              variants,
            ),
          )
        | Rec(tp, t) => Rec(tpat_map_term(tp), typ_map_term(t))
        | Forall(tp, t) => Forall(tpat_map_term(tp), typ_map_term(t))
        },
    };
    x |> f_typ(rec_call);
  };

  let rec subst = (s: t, x: TPat.t, ty: t): typ_t => {
    switch (TPat.tyvar_of_utpat(x)) {
    | Some(str) =>
      let (term, rewrap) = IdTagged.unwrap(ty);
      switch (term) {
      | Int => (Int: typ_term) |> rewrap
      | Float => Float |> rewrap
      | Bool => Bool |> rewrap
      | String => String |> rewrap
      | Unknown(prov) => Unknown(prov) |> rewrap
      | Arrow(ty1, ty2) =>
        Arrow(subst(s, x, ty1), subst(s, x, ty2)) |> rewrap
      | Prod(tys) => Prod(List.map(subst(s, x), tys)) |> rewrap
      | Sum(sm) =>
        Sum(ConstructorMap.map(Option.map(subst(s, x)), sm)) |> rewrap
      | Forall(tp2, ty)
          when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Forall(tp2, ty) |> rewrap
      | Forall(tp2, ty) => Forall(tp2, subst(s, x, ty)) |> rewrap
      | Rec(tp2, ty) when TPat.tyvar_of_utpat(x) == TPat.tyvar_of_utpat(tp2) =>
        Rec(tp2, ty) |> rewrap
      | Rec(tp2, ty) => Rec(tp2, subst(s, x, ty)) |> rewrap
      | List(ty) => List(subst(s, x, ty)) |> rewrap
      | Var(y) => str == y ? s : Var(y) |> rewrap
      | Parens(ty) => Parens(subst(s, x, ty)) |> rewrap
      | Ap(t1, t2) => Ap(subst(s, x, t1), subst(s, x, t2)) |> rewrap
      };
    | None => ty
    };
  };

  /* Type Equality: This coincides with alpha equivalence for normalized types.
     Other types may be equivalent but this will not detect so if they are not normalized. */

  let rec eq_internal = (n: int, t1: t, t2: t) => {
    switch (IdTagged.term_of(t1), IdTagged.term_of(t2)) {
    | (Parens(t1), _) => eq_internal(n, t1, t2)
    | (_, Parens(t2)) => eq_internal(n, t1, t2)
    | (Rec(x1, t1), Rec(x2, t2))
    | (Forall(x1, t1), Forall(x2, t2)) =>
      let alpha_subst =
        subst({
          term: Var("=" ++ string_of_int(n)),
          copied: false,
          ids: [Id.invalid],
        });
      eq_internal(n + 1, alpha_subst(x1, t1), alpha_subst(x2, t2));
    | (Rec(_), _) => false
    | (Forall(_), _) => false
    | (Int, Int) => true
    | (Int, _) => false
    | (Float, Float) => true
    | (Float, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (String, String) => true
    | (String, _) => false
    | (Ap(t1, t2), Ap(t1', t2')) =>
      eq_internal(n, t1, t1') && eq_internal(n, t2, t2')
    | (Ap(_), _) => false
    | (Unknown(_), Unknown(_)) => true
    | (Unknown(_), _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) =>
      eq_internal(n, t1, t1') && eq_internal(n, t2, t2')
    | (Arrow(_), _) => false
    | (Prod(tys1), Prod(tys2)) => List.equal(eq_internal(n), tys1, tys2)
    | (Prod(_), _) => false
    | (List(t1), List(t2)) => eq_internal(n, t1, t2)
    | (List(_), _) => false
    | (Sum(sm1), Sum(sm2)) =>
      /* Does not normalize the types. */
      ConstructorMap.equal(eq_internal(n), sm1, sm2)
    | (Sum(_), _) => false
    | (Var(n1), Var(n2)) => n1 == n2
    | (Var(_), _) => false
    };
  };

  let fast_equal = eq_internal(0);
}
and TPat: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let tyvar_of_utpat: t => option(string);

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = tpat_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = tpat_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | EmptyHole
        | Invalid(_)
        | Var(_) => term
        | MultiHole(things) => MultiHole(List.map(any_map_term, things))
        },
    };
    x |> f_tpat(rec_call);
  };

  let tyvar_of_utpat = ({term, _}: t) =>
    switch (term) {
    | Var(x) => Some(x)
    | _ => None
    };

  let fast_equal = (tp1: t, tp2: t) =>
    switch (tp1 |> IdTagged.term_of, tp2 |> IdTagged.term_of) {
    | (EmptyHole, EmptyHole) => true
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (MultiHole(xs), MultiHole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Var(x), Var(y)) => x == y
    | (EmptyHole, _)
    | (Invalid(_), _)
    | (MultiHole(_), _)
    | (Var(_), _) => false
    };
}
and Rul: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type term = rul_term;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = rul_t;

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
        x,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let pat_map_term =
      Pat.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let any_map_term =
      Any.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    let rec_call = ({term, _} as exp: t) => {
      ...exp,
      term:
        switch (term) {
        | Invalid(_) => term
        | Hole(things) => Hole(List.map(any_map_term, things))
        | Rules(e, rls) =>
          Rules(
            exp_map_term(e),
            List.map(
              ((p, e)) => (pat_map_term(p), exp_map_term(e)),
              rls,
            ),
          )
        },
    };
    x |> f_rul(rec_call);
  };

  let fast_equal = (r1: t, r2: t) =>
    switch (r1 |> IdTagged.term_of, r2 |> IdTagged.term_of) {
    | (Invalid(s1), Invalid(s2)) => s1 == s2
    | (Hole(xs), Hole(ys)) =>
      List.length(xs) == List.length(ys)
      && List.equal(Any.fast_equal, xs, ys)
    | (Rules(e1, rls1), Rules(e2, rls2)) =>
      Exp.fast_equal(e1, e2)
      && List.length(rls1) == List.length(rls2)
      && List.for_all2(
           ((p1, e1), (p2, e2)) =>
             Pat.fast_equal(p1, p2) && Exp.fast_equal(e1, e2),
           rls1,
           rls2,
         )
    | (Invalid(_), _)
    | (Hole(_), _)
    | (Rules(_), _) => false
    };
}

and Environment: {
  include
     (module type of VarBstMap.Ordered) with
      type t_('a) = VarBstMap.Ordered.t_('a);

  type t = environment_t;
} = {
  include VarBstMap.Ordered;

  type t = environment_t;
}

and ClosureEnvironment: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = closure_environment_t;

  let wrap: (Id.t, Environment.t) => t;

  let id_of: t => Id.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, Exp.t));

  let of_environment: Environment.t => t;

  let id_equal: (closure_environment_t, closure_environment_t) => bool;

  let empty: t;
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(Exp.t);
  let contains: (t, Var.t) => bool;
  let update: (Environment.t => Environment.t, t) => t;
  let update_keep_id: (Environment.t => Environment.t, t) => t;
  let extend: (t, (Var.t, Exp.t)) => t;
  let extend_keep_id: (t, (Var.t, Exp.t)) => t;
  let union: (t, t) => t;
  let union_keep_id: (t, t) => t;
  let map: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let map_keep_id: (((Var.t, Exp.t)) => Exp.t, t) => t;
  let filter: (((Var.t, Exp.t)) => bool, t) => t;
  let filter_keep_id: (((Var.t, Exp.t)) => bool, t) => t;
  let fold: (((Var.t, Exp.t), 'b) => 'b, 'b, t) => 'b;

  let without_keys: (list(Var.t), t) => t;
  let with_symbolic_keys: (list(Var.t), t) => t;

  let placeholder: t;
} = {
  module Inner: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = closure_environment_t;

    let wrap: (Id.t, Environment.t) => t;

    let id_of: t => Id.t;
    let map_of: t => Environment.t;
  } = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = closure_environment_t;

    let wrap = (ei, map): t => (ei, map);

    let id_of = ((ei, _)) => ei;
    let map_of = ((_, map)) => map;
    let (sexp_of_t, t_of_sexp) =
      StructureShareSexp.structure_share_here(id_of, sexp_of_t, t_of_sexp);
  };
  include Inner;

  let to_list = env => env |> map_of |> Environment.to_listo;

  let of_environment = map => {
    let ei = Id.mk();
    wrap(ei, map);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  let empty = Environment.empty |> of_environment;

  let is_empty = env => env |> map_of |> Environment.is_empty;

  let length = env => Environment.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => Environment.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => Environment.contains(map, x));

  let update = (f, env) => env |> map_of |> f |> of_environment;

  let update_keep_id = (f, env) => env |> map_of |> f |> wrap(env |> id_of);

  let extend = (env, xr) =>
    env |> update(map => Environment.extend(map, xr));

  let extend_keep_id = (env, xr) =>
    env |> update_keep_id(map => Environment.extend(map, xr));

  let union = (env1, env2) =>
    env2 |> update(map2 => Environment.union(env1 |> map_of, map2));

  let union_keep_id = (env1, env2) =>
    env2 |> update_keep_id(map2 => Environment.union(env1 |> map_of, map2));

  let map = (f, env) => env |> update(Environment.mapo(f));

  let map_keep_id = (f, env) => env |> update_keep_id(Environment.mapo(f));

  let filter = (f, env) => env |> update(Environment.filtero(f));

  let filter_keep_id = (f, env) =>
    env |> update_keep_id(Environment.filtero(f));

  let fold = (f, init, env) => env |> map_of |> Environment.foldo(f, init);

  let placeholder = wrap(Id.invalid, Environment.empty);

  let without_keys = keys => update(Environment.without_keys(keys));
  let with_symbolic_keys = (keys, env) =>
    List.fold_right(
      (key, env) => extend(env, (key, Var(key) |> IdTagged.fresh)),
      keys,
      env,
    );
}
and StepperFilterKind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map_term:
    (
      ~f_exp: (Exp.t => Exp.t, Exp.t) => Exp.t=?,
      ~f_pat: (Pat.t => Pat.t, Pat.t) => Pat.t=?,
      ~f_typ: (Typ.t => Typ.t, Typ.t) => Typ.t=?,
      ~f_tpat: (TPat.t => TPat.t, TPat.t) => TPat.t=?,
      ~f_rul: (Rul.t => Rul.t, Rul.t) => Rul.t=?,
      ~f_any: (Any.t => Any.t, Any.t) => Any.t=?,
      t
    ) =>
    t;

  let map: (Exp.t => Exp.t, t) => t;

  let fast_equal: (t, t) => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = stepper_filter_kind_t;

  let map = (mapper, filter: t): t => {
    switch (filter) {
    | Filter({act, pat}) => Filter({act, pat: mapper(pat)})
    | Residue(idx, act) => Residue(idx, act)
    };
  };

  let map_term =
      (
        ~f_exp=continue,
        ~f_pat=continue,
        ~f_typ=continue,
        ~f_tpat=continue,
        ~f_rul=continue,
        ~f_any=continue,
      ) => {
    let exp_map_term =
      Exp.map_term(~f_exp, ~f_pat, ~f_typ, ~f_tpat, ~f_rul, ~f_any);
    (
      fun
      | Filter({pat: e, act}) => Filter({pat: exp_map_term(e), act})
      | Residue(i, a) => Residue(i, a):
        t => t
    );
  };

  let fast_equal = (f1: t, f2: t) =>
    switch (f1, f2) {
    | (Filter({pat: e1, act: a1}), Filter({pat: e2, act: a2})) =>
      Exp.fast_equal(e1, e2) && a1 == a2
    | (Residue(i1, a1), Residue(i2, a2)) => i1 == i2 && a1 == a2
    | (Filter(_), _)
    | (Residue(_), _) => false
    };
};

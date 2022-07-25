open Sexplib.Std;

module rec DHExp: {
  module BinBoolOp: {
    [@deriving sexp]
    type t =
      | And
      | Or;
    let of_op: Operators_Exp.t => option(t);
    let to_op: t => Operators_Exp.t;
  };

  module BinIntOp: {
    [@deriving sexp]
    type t =
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals;
    let of_op: Operators_Exp.t => option((t, HTyp.t));
    let to_op: t => Operators_Exp.t;
  };

  module BinFloatOp: {
    [@deriving sexp]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals;
    let of_op: Operators_Exp.t => option((t, HTyp.t));
    let to_op: t => Operators_Exp.t;
  };

  [@deriving sexp]
  type t =
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    | Closure(ClosureEnvironment.t, t)
    | BoundVar(Var.t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, HTyp.t, t)
    | Fun(DHPat.t, HTyp.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | ListNil(HTyp.t)
    | Cons(t, t)
    | Inj(HTyp.t, InjSide.t, t)
    | Pair(t, t)
    | Triv
    | ConsistentCase(case)
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string: t => string;

  let mk_tuple: list(t) => t;

  let cast: (t, HTyp.t, HTyp.t) => t;

  let apply_casts: (t, list((HTyp.t, HTyp.t))) => t;

  let fast_equal: (t, t) => bool;
} = {
  module BinBoolOp = {
    [@deriving sexp]
    type t =
      | And
      | Or;

    let of_op = (op: UHExp.operator): option(t) =>
      switch (op) {
      | And => Some(And)
      | Or => Some(Or)
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (op: t): UHExp.operator =>
      switch (op) {
      | And => And
      | Or => Or
      };
  };

  module BinIntOp = {
    [@deriving sexp]
    type t =
      | Minus
      | Plus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals;

    let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
      switch (op) {
      | Minus => Some((Minus, Int))
      | Plus => Some((Plus, Int))
      | Times => Some((Times, Int))
      | Divide => Some((Divide, Int))
      | LessThan => Some((LessThan, Bool))
      | GreaterThan => Some((GreaterThan, Bool))
      | Equals => Some((Equals, Bool))
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals
      | And
      | Or
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (bio: t): UHExp.operator =>
      switch (bio) {
      | Minus => Minus
      | Plus => Plus
      | Times => Times
      | Divide => Divide
      | LessThan => LessThan
      | GreaterThan => GreaterThan
      | Equals => Equals
      };
  };

  module BinFloatOp = {
    [@deriving sexp]
    type t =
      | FPlus
      | FMinus
      | FTimes
      | FDivide
      | FLessThan
      | FGreaterThan
      | FEquals;

    let of_op = (op: UHExp.operator): option((t, HTyp.t)) =>
      switch (op) {
      | FPlus => Some((FPlus, Float))
      | FMinus => Some((FMinus, Float))
      | FTimes => Some((FTimes, Float))
      | FDivide => Some((FDivide, Float))
      | FLessThan => Some((FLessThan, Bool))
      | FGreaterThan => Some((FGreaterThan, Bool))
      | FEquals => Some((FEquals, Bool))
      | Plus
      | Minus
      | Times
      | Divide
      | LessThan
      | GreaterThan
      | Equals
      | And
      | Or
      | Space
      | Cons
      | Comma => None
      };

    let to_op = (bfo: t): UHExp.operator =>
      switch (bfo) {
      | FPlus => FPlus
      | FMinus => FMinus
      | FTimes => FTimes
      | FDivide => FDivide
      | FLessThan => FLessThan
      | FGreaterThan => FGreaterThan
      | FEquals => FEquals
      };
  };

  [@deriving sexp]
  type t =
    /* Hole types */
    | EmptyHole(MetaVar.t, HoleInstanceId.t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | ExpandingKeyword(MetaVar.t, HoleInstanceId.t, ExpandingKeyword.t)
    | FreeVar(MetaVar.t, HoleInstanceId.t, Var.t)
    | InvalidText(MetaVar.t, HoleInstanceId.t, string)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
    /* Generalized closures */
    | Closure(ClosureEnvironment.t, t)
    /* Other expressions forms */
    | BoundVar(Var.t)
    | Let(DHPat.t, t, t)
    | FixF(Var.t, HTyp.t, t)
    | Fun(DHPat.t, HTyp.t, t)
    | Ap(t, t)
    | ApBuiltin(string, list(t))
    | BoolLit(bool)
    | IntLit(int)
    | FloatLit(float)
    | BinBoolOp(BinBoolOp.t, t, t)
    | BinIntOp(BinIntOp.t, t, t)
    | BinFloatOp(BinFloatOp.t, t, t)
    | ListNil(HTyp.t)
    | Cons(t, t)
    | Inj(HTyp.t, InjSide.t, t)
    | Pair(t, t)
    | Triv
    | ConsistentCase(case)
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t)
    | InvalidOperation(t, InvalidOperationError.t)
  and case =
    | Case(t, list(rule), int)
  and rule =
    | Rule(DHPat.t, t);

  let constructor_string = (d: t): string =>
    switch (d) {
    | EmptyHole(_, _) => "EmptyHole"
    | NonEmptyHole(_, _, _, _) => "NonEmptyHole"
    | ExpandingKeyword(_, _, _) => "ExpandingKeyword"
    | FreeVar(_, _, _) => "FreeVar"
    | InvalidText(_) => "InvalidText"
    | BoundVar(_) => "BoundVar"
    | Let(_, _, _) => "Let"
    | FixF(_, _, _) => "FixF"
    | Fun(_, _, _) => "Fun"
    | Closure(_, _) => "Closure"
    | Ap(_, _) => "Ap"
    | ApBuiltin(_, _) => "ApBuiltin"
    | BoolLit(_) => "BoolLit"
    | IntLit(_) => "IntLit"
    | FloatLit(_) => "FloatLit"
    | BinBoolOp(_, _, _) => "BinBoolOp"
    | BinIntOp(_, _, _) => "BinIntOp"
    | BinFloatOp(_, _, _) => "BinFloatOp"
    | ListNil(_) => "ListNil"
    | Cons(_, _) => "Cons"
    | Inj(_, _, _) => "Inj"
    | Pair(_, _) => "Pair"
    | Triv => "Triv"
    | ConsistentCase(_) => "ConsistentCase"
    | InconsistentBranches(_, _, _) => "InconsistentBranches"
    | Cast(_, _, _) => "Cast"
    | FailedCast(_, _, _) => "FailedCast"
    | InvalidOperation(_) => "InvalidOperation"
    };

  let rec mk_tuple: list(t) => t =
    fun
    | [] => failwith("mk_tuple: expected at least 1 element")
    | [d] => d
    | [d, ...ds] => Pair(d, mk_tuple(ds));

  let cast = (d: t, t1: HTyp.t, t2: HTyp.t): t =>
    if (HTyp.eq(t1, t2)) {
      d;
    } else {
      Cast(d, t1, t2);
    };

  let apply_casts = (d: t, casts: list((HTyp.t, HTyp.t))): t =>
    List.fold_left(
      (d, c: (HTyp.t, HTyp.t)) => {
        let (ty1, ty2) = c;
        cast(d, ty1, ty2);
      },
      d,
      casts,
    );

  let rec fast_equal = (d1: t, d2: t): bool => {
    switch (d1, d2) {
    /* Primitive forms: regular structural equality */
    | (BoundVar(_), _)
    | (BoolLit(_), _)
    | (IntLit(_), _)
    | (FloatLit(_), _)
    | (ListNil(_), _)
    | (Triv, _) => d1 == d2

    /* Non-hole forms: recurse */
    | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
      dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (FixF(f1, ty1, d1), FixF(f2, ty2, d2)) =>
      f1 == f2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Fun(dp1, ty1, d1), Fun(dp2, ty2, d2)) =>
      dp1 == dp2 && ty1 == ty2 && fast_equal(d1, d2)
    | (Ap(d11, d21), Ap(d12, d22))
    | (Cons(d11, d21), Cons(d12, d22))
    | (Pair(d11, d21), Pair(d12, d22)) =>
      fast_equal(d11, d12) && fast_equal(d21, d22)
    | (ApBuiltin(f1, args1), ApBuiltin(f2, args2)) =>
      f1 == f2 && List.for_all2(fast_equal, args1, args2)
    | (BinBoolOp(op1, d11, d21), BinBoolOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinIntOp(op1, d11, d21), BinIntOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (BinFloatOp(op1, d11, d21), BinFloatOp(op2, d12, d22)) =>
      op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
    | (Inj(ty1, side1, d1), Inj(ty2, side2, d2)) =>
      ty1 == ty2 && side1 == side2 && fast_equal(d1, d2)
    | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
    | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
      fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
    | (InvalidOperation(d1, reason1), InvalidOperation(d2, reason2)) =>
      fast_equal(d1, d2) && reason1 == reason2
    | (ConsistentCase(case1), ConsistentCase(case2)) =>
      fast_equal_case(case1, case2)
    /* We can group these all into a `_ => false` clause; separating
       these so that we get exhaustiveness checking. */
    | (Let(_), _)
    | (FixF(_), _)
    | (Fun(_), _)
    | (Ap(_), _)
    | (ApBuiltin(_), _)
    | (Cons(_), _)
    | (Pair(_), _)
    | (BinBoolOp(_), _)
    | (BinIntOp(_), _)
    | (BinFloatOp(_), _)
    | (Inj(_), _)
    | (Cast(_), _)
    | (FailedCast(_), _)
    | (InvalidOperation(_), _)
    | (ConsistentCase(_), _) => false

    /* Hole forms: when checking environments, only check that
       environment ID's are equal, don't check structural equality.

       (This resolves a performance issue with many nested holes.) */
    | (EmptyHole(u1, i1), EmptyHole(u2, i2)) => u1 == u2 && i1 == i2
    | (NonEmptyHole(reason1, u1, i1, d1), NonEmptyHole(reason2, u2, i2, d2)) =>
      reason1 == reason2 && u1 == u2 && i1 == i2 && fast_equal(d1, d2)
    | (ExpandingKeyword(u1, i1, kw1), ExpandingKeyword(u2, i2, kw2)) =>
      u1 == u2 && i1 == i2 && kw1 == kw2
    | (FreeVar(u1, i1, x1), FreeVar(u2, i2, x2)) =>
      u1 == u2 && i1 == i2 && x1 == x2
    | (InvalidText(u1, i1, text1), InvalidText(u2, i2, text2)) =>
      u1 == u2 && i1 == i2 && text1 == text2
    | (Closure((ei1, _), d1), Closure((ei2, _), d2)) =>
      /* Cannot use ClosureEnvironment.equals here because it will create a dependency loop. */
      ei1 == ei2 && fast_equal(d1, d2)
    | (
        InconsistentBranches(u1, i1, case1),
        InconsistentBranches(u2, i2, case2),
      ) =>
      u1 == u2 && i1 == i2 && fast_equal_case(case1, case2)
    | (EmptyHole(_), _)
    | (NonEmptyHole(_), _)
    | (ExpandingKeyword(_), _)
    | (FreeVar(_), _)
    | (InvalidText(_), _)
    | (Closure(_), _)
    | (InconsistentBranches(_), _) => false
    };
  }
  and fast_equal_case = (Case(d1, rules1, i1), Case(d2, rules2, i2)) => {
    fast_equal(d1, d2)
    && List.length(rules1) == List.length(rules2)
    && List.for_all2(
         (Rule(dp1, d1), Rule(dp2, d2)) =>
           dp1 == dp2 && fast_equal(d1, d2),
         rules1,
         rules2,
       )
    && i1 == i2;
  };
}

and Environment: {
  include  (module type of VarBstMap) with type t_('a) = VarBstMap.t_('a);

  [@deriving sexp]
  type t = t_(DHExp.t);
} = {
  include VarBstMap;

  [@deriving sexp]
  type t = t_(DHExp.t);
}

and ClosureEnvironment: {
  [@deriving sexp]
  type t = (EnvironmentId.t, Environment.t);

  let id_of: t => EnvironmentId.t;
  let map_of: t => Environment.t;

  let to_list: t => list((Var.t, DHExp.t));

  let of_environment:
    (Environment.t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);

  let id_equal: (t, t) => bool;

  let empty: EnvironmentIdGen.t => (t, EnvironmentIdGen.t);
  let is_empty: t => bool;
  let length: t => int;

  let lookup: (t, Var.t) => option(DHExp.t);
  let contains: (t, Var.t) => bool;
  let extend:
    (t, (Var.t, DHExp.t), EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let union: (t, t, EnvironmentIdGen.t) => (t, EnvironmentIdGen.t);
  let map:
    (((Var.t, DHExp.t)) => DHExp.t, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);
  let map_keep_id: (((Var.t, DHExp.t)) => DHExp.t, t) => t;
  let filter:
    (((Var.t, DHExp.t)) => bool, t, EnvironmentIdGen.t) =>
    (t, EnvironmentIdGen.t);

  let placeholder: t;
} = {
  [@deriving sexp]
  type t = (EnvironmentId.t, Environment.t);

  let id_of = ((ei, _)) => ei;
  let map_of = ((_, map)) => map;

  let to_list = ((_, map)) => map |> VarBstMap.to_list;

  let of_environment = (map, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, map), eig);
  };

  /* Equals only needs to check environment id's (faster than structural equality
   * checking.) */
  let id_equal = (env1, env2) => id_of(env1) == id_of(env2);

  /* FIXME: Use of_environment. */
  let empty = eig => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, VarBstMap.empty), eig);
  };

  let is_empty = env => env |> map_of |> VarBstMap.is_empty;

  let length = env => VarBstMap.length(map_of(env));

  let lookup = (env, x) =>
    env |> map_of |> (map => VarBstMap.lookup(map, x));

  let contains = (env, x) =>
    env |> map_of |> (map => VarBstMap.contains(map, x));

  let extend = (env, xr, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, VarBstMap.extend(map_of(env), xr)), eig);
  };

  let union = (env1, env2, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, VarBstMap.union(map_of(env1), map_of(env2))), eig);
  };

  let map = (f, env, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, env |> map_of |> VarBstMap.map(f)), eig);
  };

  let map_keep_id = (f, env) => (
    id_of(env),
    VarBstMap.map(f, map_of(env)),
  );

  let filter = (f, env, eig) => {
    let (ei, eig) = EnvironmentIdGen.next(eig);
    ((ei, env |> map_of |> VarBstMap.filter(f)), eig);
  };

  let placeholder = (EnvironmentId.invalid, VarBstMap.empty);
};

open Sexplib.Std;

[@deriving sexp]
type t =
  | EmptyHole(MetaVar.t, MetaVarInst.t)
  | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, t)
  | Wild
  // TODO rename to ExpandingKeyword
  | Keyword(MetaVar.t, MetaVarInst.t, ExpandingKeyword.t)
  | InvalidText(MetaVar.t, MetaVarInst.t, string)
  | Var(Var.t)
  | IntLit(int)
  | FloatLit(float)
  | BoolLit(bool)
  | Inj(inj)
  | InjError(InjErrStatus.HoleReason.t, MetaVar.t, MetaVarInst.t, inj)
  | ListNil
  | Cons(t, t)
  | Pair(t, t)
  | Triv /* unit intro */
  | Ap(t, t)
and inj = (UHTag.t, option(t));

let rec mk_tuple: list(t) => t =
  fun
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [dp] => dp
  | [dp, ...dps] => Pair(dp, mk_tuple(dps));

/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (x: Var.t, dp: t): bool =>
  switch (dp) {
  | EmptyHole(_, _)
  | NonEmptyHole(_, _, _, _)
  | Wild
  | InvalidText(_)
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | Triv
  | ListNil
  | Keyword(_, _, _) => false
  | Var(y) => Var.eq(x, y)
  | Inj((_, Some(dp1))) => binds_var(x, dp1)
  | Inj((_, None)) => false
  | InjError(_, _, _, (_, Some(dp1))) => binds_var(x, dp1)
  | InjError(_, _, _, (_, None)) => false
  | Pair(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Cons(dp1, dp2) => binds_var(x, dp1) || binds_var(x, dp2)
  | Ap(_, _) => false
  };

[@deriving sexp]
type t =
  | OutOfFuel
  | FreeInvalidVar(Var.t)
  | BadPatternMatch
  | CastBVHoleGround(DHExp.t)
  | InvalidBoxedLam(DHExp.t)
  | InvalidBoxedBoolLit(DHExp.t)
  | InvalidBoxedIntLit(DHExp.t)
  | InvalidBoxedFloatLit(DHExp.t)
  | InvalidBoxedStringLit(DHExp.t)
  | InvalidBuiltin(string)
  | BadBuiltinAp(string, list(DHExp.t));

[@deriving sexp]
exception Exception(t);

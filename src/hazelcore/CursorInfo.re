open Sexplib.Std;

[@deriving sexp]
type join_of_branches =
  | NoBranches
  // steps to the case
  | InconsistentBranchTys(list(HTyp.t), CursorPath.steps)
  | JoinTy(HTyp.t);

[@deriving sexp]
type typed =
  // cursor is on a lambda with an argument type annotation
  /* cursor in analytic position */
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  // cursor is on a type inconsistent expression
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on a tuple of the wrong length
  | AnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on a free variable
  | AnaFree(HTyp.t)
  // cursor is on invalid text
  | AnaInvalid(HTyp.t)
  // cursor is on a keyword
  | AnaKeyword(HTyp.t, ExpandingKeyword.t)
  // cursor is on an injection and expected type is not a sum
  | AnaInjExpectedTypeNotConsistenWithSums(HTyp.t)
  // cursor is on an injection with a bad tag
  | AnaInjExpectedArg(HTyp.t)
  // cursor is on a nullary injection with argument
  | AnaInjUnexpectedArg(HTyp.t)
  // none of the above and didn't go through subsumption
  | Analyzed(HTyp.t)
  // none of the above and went through subsumption
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic position */
  // cursor is on the function position of an ap,
  // and that expression does not synthesize a type
  // with a matched arrow type
  | SynErrorArrow
      // expected
      (
        HTyp.t,
        // got
        HTyp.t,
      )
  // cursor is on the function position of an ap,
  // and that expression does synthesize a type
  // with a matched arrow type
  | SynMatchingArrow(HTyp.t, HTyp.t)
  // cursor is on a free variable in the function
  // position of an ap
  | SynFreeArrow(HTyp.t)
  // cursor is on a keyword in the function position of an ap
  | SynKeywordArrow(HTyp.t, ExpandingKeyword.t)
  // cursor is on a case with inconsistent branch types
  // in the function position of an ap
  | SynInconsistentBranchesArrow(list(HTyp.t), CursorPath.steps)
  // cursor is on invalid text in the fuction position of an ap
  | SynInvalidArrow(HTyp.t)
  // cursor is on invalid text
  | SynInvalid
  // none of the above, cursor is on a free variable
  | SynFree
  // cursor is on a keyword
  | SynKeyword(ExpandingKeyword.t)
  // cursor is on an injection in synthetic position
  | SynInjection
  // cursor is on the clause of a case
  | SynBranchClause
      // lub of other branches
      (
        join_of_branches,
        // info for the clause
        typed,
        // index of the branch
        int,
      )
  // cursor is on a case with branches of inconsistent types
  // keep track of steps to form that contains the branches
  | SynInconsistentBranches(list(HTyp.t), CursorPath.steps)
  // none of the above
  | Synthesized(HTyp.t)
  /* cursor in analytic pattern position */
  // cursor is on a type inconsistent pattern
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on an inj pattern and expected type is not a sum
  | PatAnaInjExpectedTypeNotConsistentWithSums(HTyp.t)
  // cursor is on an injection pattern with a bad tag
  | PatAnaInjBadTag(UHTag.t)
  // cursor is on a unary injection pattern with no body
  | PatAnaInjExpectedBody(HTyp.t)
  // cursor is on a nullary injection pattern with body
  | PatAnaInjUnexpectedBody
  // cursor is on a tuple pattern of the wrong length
  | PatAnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on invalid text
  | PatAnaInvalid(HTyp.t)
  // cursor is on a keyword
  | PatAnaKeyword(HTyp.t, ExpandingKeyword.t)
  // none of the above and didn't go through subsumption
  | PatAnalyzed(HTyp.t)
  // none of the above and went through subsumption
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic pattern position */
  // cursor is on a keyword
  | PatSynthesized(HTyp.t)
  | PatSynKeyword(ExpandingKeyword.t)
  /* cursor in type position */
  | OnType
  /* cursor in tag position */
  | OnTag
  | OnTagHole
  | OnInvalidTag(UHTag.t)
  | OnUnknownTag(UHTag.t)
  | OnDuplicateTag(UHTag.t)
  /* cursor in sum body position */
  | OnSumBody
  /* (we will have a richer structure here later)*/
  | OnLine
  | OnRule;

[@deriving sexp]
type cursor_term =
  | Exp(CursorPosition.t, UHExp.operand)
  | Pat(CursorPosition.t, UHPat.operand)
  | Typ(CursorPosition.t, UHTyp.operand)
  | Tag(CursorPosition.t, UHTag.t)
  | ExpOp(CursorPosition.t, UHExp.operator)
  | PatOp(CursorPosition.t, UHPat.operator)
  | TypOp(CursorPosition.t, UHTyp.operator)
  | SumBody(CursorPosition.t, UHTyp.sumbody_operand)
  | SumBodyOp(CursorPosition.t, UHTyp.sumbody_operator)
  | Line(CursorPosition.t, UHExp.line)
  | Rule(CursorPosition.t, UHExp.rule);

// TODO refactor into variants
// based on term sort and shape
[@deriving sexp]
type t = {
  cursor_term,
  typed,
  ctx: Contexts.t,
  // hack while merging
  uses: option(UsageAnalysis.uses_list),
};

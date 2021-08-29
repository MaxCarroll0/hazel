open OptUtil.Syntax;

let hole_not_empty = (hi: CursorPath.hole_info) =>
  switch (hi.sort) {
  | ExpHole(_, TypeErr | VarErr)
  | PatHole(_, TypeErr | VarErr) => true
  | _ => false
  };

let err_holes = (ze: ZExp.t): list(CursorPath.hole_info) =>
  CursorPath_Exp.holes(ZExp.erase(ze), [], [])
  |> List.filter(hole_not_empty);

let idomaticity_score_parent =
    (action: Action.t, enclosing_zoperand: CursorInfo.enclosing_zoperand): int => {
  switch (action, enclosing_zoperand) {
  | (ReplaceOperand(operand, None), Some(parent_operand)) =>
    switch (parent_operand) {
    | CaseZE(_) =>
      switch (operand) {
      | Case(_) => (-2)
      | Lam(_) => (-3)
      | Inj(_) => (-1)
      | IntLit(_)
      | BoolLit(_)
      | FloatLit(_)
      | ListNil(_) => (-1)
      | _ => 0
      }
    /* parenthesized apps */
    | ParenthesizedZ((
        [],
        ExpLineZ(ZOpSeq(_, ZOperand(CursorE(_), (E, A(Space, _))))),
        [],
      )) =>
      /* lits/inj don't really matter as the type will never match */
      switch (operand) {
      | Case(_) => (-3)
      | Lam(_) => (-2)
      | Parenthesized(_) => (-1)
      | _ => 0
      }
    | _ => 0
    }
  | _ => 0
  };
};

let type_specificity_score =
    (expected_ty: HTyp.t, res_ty: HTyp.t, actual_ty: option(HTyp.t)) =>
  // improve society somewhat
  switch (expected_ty, res_ty, actual_ty) {
  | (Hole, _, _) => 0
  | (_, Hole, Some(Hole) | None) => 0
  | (_, Hole, _) => (-1)
  | (_, x, Some(Hole)) when x != Hole => 1
  | _ => 0
  };

let opseq_report =
    (action: Action.t, {ctx, enclosing_zopseq, _}: CursorInfo.t) => {
  let* (opseq_expected_ty, old_zexp) =
    switch (enclosing_zopseq) {
    | ExpSeq(zopseq, expected_ty) =>
      let expected_ty =
        switch (expected_ty) {
        | None => HTyp.Hole
        | Some(ty) => ty
        };
      Some((expected_ty, zopseq |> ZExp.ZBlock.wrap'));
    | _ =>
      print_endline("Warning: opseq_report: no zopseq provided");
      None;
    };
  let+ (actual_ty, new_zexp) =
    switch (
      Action_Exp.syn_perform(
        ctx,
        action,
        (old_zexp, opseq_expected_ty, MetaVarGen.init),
      )
    ) {
    | Failed
    | CursorEscaped(_) =>
      print_endline("Warning: opseq_report: syn_perform failure");
      None;
    | Succeeded((new_zexp, new_type, _)) => Some((new_type, new_zexp))
    };
  let context_consistent_after =
    HTyp.consistent(opseq_expected_ty, actual_ty);
  let internal_errors_before = old_zexp |> err_holes |> List.length;
  let internal_errors_after = new_zexp |> err_holes |> List.length;
  (context_consistent_after, internal_errors_before, internal_errors_after);
};

let check_suggestion =
    (
      action: Action.t,
      res_ty: HTyp.t,
      {enclosing_zoperand, expected_ty, actual_ty, _} as ci: CursorInfo.t,
    )
    : option(Suggestion.score) => {
  let+ (
    context_consistent_after,
    internal_errors_before,
    internal_errors_after,
  ) =
    opseq_report(action, ci);
  let context_errors =
    switch (context_consistent_after) {
    | false => (-1)
    | true => 0
    };
  let internal_errors = internal_errors_before - internal_errors_after;
  let delta_errors = internal_errors + context_errors;
  let idiomaticity = idomaticity_score_parent(action, enclosing_zoperand);
  let type_specificity =
    type_specificity_score(expected_ty, res_ty, actual_ty);
  Suggestion.{idiomaticity, type_specificity, delta_errors};
};

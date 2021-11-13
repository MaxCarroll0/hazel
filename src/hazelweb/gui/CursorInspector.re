open Virtual_dom.Vdom;

type err_state_b =
  | TypeInconsistency
  | BindingError
  | OK;

let view =
    (
      ~inject: ModelAction.t => Event.t,
      ~font_metrics: FontMetrics.t,
      ~selected_tag_hole: option(MetaVar.t),
      model: Model.t,
    )
    : Node.t => {
  let typebar = ty =>
    Node.div(
      [Attr.classes(["infobar", "typebar"])],
      [HTypCode.view(~inject, ~font_metrics, ~selected_tag_hole, ty)],
    );
  let tagbar = tag =>
    Node.div(
      [Attr.classes(["infobar", "tagbar"])],
      [HTypCode_Tag.view(~inject, ~font_metrics, ~selected_tag_hole, tag)],
    );
  let matched_ty_bar = (ty1, ty2) =>
    Node.div(
      [Attr.classes(["infobar", "matched-type-bar"])],
      [
        HTypCode.view(~inject, ~font_metrics, ~selected_tag_hole, ty1),
        Node.span(
          [Attr.classes(["matched-connective"])],
          [Node.text(" ▶ ")],
        ),
        HTypCode.view(~inject, ~font_metrics, ~selected_tag_hole, ty2),
      ],
    );
  let inconsistent_branches_ty_bar =
      (branch_types, path_to_case, skipped_index) =>
    Node.div(
      [Attr.classes(["infobar", "inconsistent-branches-ty-bar"])],
      List.mapi(
        (index, ty) => {
          let shifted_index =
            switch (skipped_index) {
            | None => index
            | Some(skipped_index) =>
              if (index >= skipped_index) {
                index + 1;
              } else {
                index;
              }
            };
          Node.span(
            [
              Attr.on_click(_ => {
                inject(SelectCaseBranch(path_to_case, shifted_index))
              }),
            ],
            [HTypCode.view(~inject, ~font_metrics, ~selected_tag_hole, ty)],
          );
        },
        branch_types,
      ),
    );

  let special_msg_bar = (msg: string) =>
    Node.div(
      [Attr.classes(["infobar", "special-msg-bar"])],
      [Node.text(msg)],
    );

  let expected_indicator = (title_text, type_div) =>
    Node.div(
      [Attr.classes(["indicator", "expected-indicator"])],
      [Panel.view_of_main_title_bar(title_text), type_div],
    );
  let expected_ty_title = "Expecting an expression of type";
  let expected_ty_title_pat = "Expecting a pattern of type";
  let expected_ty_title_consistent = "Expecting an expression consistent with type";
  let expected_ty_indicator = ty =>
    expected_indicator(expected_ty_title, typebar(ty));
  let expected_ty_indicator_pat = ty =>
    expected_indicator(expected_ty_title_pat, typebar(ty));
  let expected_ty_indicator_consistent = ty =>
    expected_indicator(expected_ty_title_consistent, typebar(ty));
  let expected_msg_indicator = msg =>
    expected_indicator("Expecting an expression of ", special_msg_bar(msg));
  let expected_msg_indicator_pat = msg =>
    expected_indicator("Expecting a pattern of ", special_msg_bar(msg));
  let expected_any_indicator = expected_msg_indicator("any type");
  let expected_any_indicator_pat = expected_msg_indicator_pat("any type");
  let expected_a_type_indicator =
    expected_indicator("Expecting ", special_msg_bar("a type"));
  let expected_a_tag_indicator =
    expected_indicator("Expecting ", special_msg_bar("a tag"));
  let expected_a_known_tag_indicator =
    expected_indicator("Expecting ", special_msg_bar("a known tag"));
  let expected_a_unique_tag_indicator =
    expected_indicator("Expecting ", special_msg_bar("a unique tag"));
  let expected_a_sum_body_indicator =
    expected_indicator("Expecting ", special_msg_bar("a sum body"));
  let expected_a_line_indicator =
    expected_indicator("Expecting ", special_msg_bar("a line item"));
  let expected_a_rule_indicator =
    expected_indicator("Expecting ", special_msg_bar("a case rule"));
  let expected_inconsistent_branches_indicator =
      (branch_types, path_to_case, skipped_index) =>
    expected_indicator(
      "No consistent expected type",
      inconsistent_branches_ty_bar(
        branch_types,
        path_to_case,
        Some(skipped_index),
      ),
    );
  let expected_nothing_indicator = expected_msg_indicator("nothing");
  let expected_type_consistent_with_sums_indicator =
    expected_msg_indicator("a type consistent with sums");
  let expected_member_of_sumtype_indicator =
    expected_msg_indicator("a member of the expected sum type");
  let expected_injection_body_indicator = ty =>
    expected_indicator(
      "Expecting an injection body expression of ",
      typebar(ty),
    );
  let expected_injection_body_indicator_pat = ty =>
    expected_indicator(
      "Expecting an injection body pattern of ",
      typebar(ty),
    );

  let got_indicator = (title_text, type_div) =>
    Node.div(
      [Attr.classes(["indicator", "got-indicator"])],
      [Panel.view_of_other_title_bar(title_text), type_div],
    );
  let got_ty_indicator = ty => got_indicator("Got type", typebar(ty));
  let got_tag_indicator = t => got_indicator("Got tag", tagbar(t));
  let got_nothing_indicator =
    got_indicator("Got nothing", typebar(HTyp.Hole));
  let got_something_indicator =
    got_indicator("Got something", typebar(HTyp.Hole));
  let got_as_expected_ty_indicator = ty =>
    got_indicator("Got as expected", typebar(ty));
  let got_inconsistent_indicator = got_ty =>
    got_indicator("Got inconsistent type", typebar(got_ty));
  let got_inconsistent_matched_indicator = (got_ty, matched_ty) =>
    got_indicator(
      "Got inconsistent type ▶ assumed ",
      matched_ty_bar(got_ty, matched_ty),
    );
  let got_inconsistent_branches_indicator = (branch_types, path_to_case) =>
    got_indicator(
      "Got inconsistent branch types",
      inconsistent_branches_ty_bar(branch_types, path_to_case, None),
    );

  let got_free_indicator =
    got_indicator("Got a free variable", typebar(HTyp.Hole));

  let got_invalid_indicator =
    got_indicator("Got invalid text", typebar(HTyp.Hole));

  let got_invalid_tag_indicator = (tag: UHTag.t) =>
    got_indicator("Got invalid tag", tagbar(tag));

  let got_unknown_tag_indicator = (tag: UHTag.t) =>
    got_indicator("Got unknown tag", tagbar(tag));

  let got_duplicate_tag_indicator = (tag: UHTag.t) =>
    got_indicator("Got duplicate tag", tagbar(tag));

  let got_consistent_indicator = got_ty =>
    got_indicator("Got consistent type", typebar(got_ty));
  let got_a_type_indicator = got_indicator("Got", special_msg_bar("a type"));
  let got_a_tag_indicator = got_indicator("Got", special_msg_bar("a tag"));
  let got_a_tag_hole_indicator =
    got_indicator("Got", special_msg_bar("a tag hole"));
  let got_a_sum_type_body_indicator =
    got_indicator("Got", special_msg_bar("a sum type body"));
  let got_a_line_indicator =
    got_indicator("Got", special_msg_bar("a line item"));
  let got_a_rule_indicator =
    got_indicator("Got", special_msg_bar("a case rule"));
  let got_keyword_indicator =
    got_indicator("Got a reserved keyword", typebar(HTyp.Hole));

  let ci = model |> Model.get_program |> Program.get_cursor_info;
  let rec get_indicator_info = (typed: CursorInfo.typed) =>
    switch (typed) {
    | Analyzed(ty) =>
      let ind1 = expected_ty_indicator(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK);
    | AnaAnnotatedLambda(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | AnaTypeInconsistent(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      (ind1, ind2, TypeInconsistency);
    | AnaWrongLength(expected_len, got_len, _expected_ty) =>
      let expected_msg = string_of_int(expected_len) ++ "-tuple";
      let ind1 =
        expected_indicator(
          "Expecting an expression of type",
          special_msg_bar(expected_msg),
        );
      let got_msg = string_of_int(got_len) ++ "-tuple";
      let ind2 =
        got_indicator(
          "Got tuple of the wrong length",
          special_msg_bar(got_msg),
        );
      (ind1, ind2, TypeInconsistency);
    | AnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | AnaFree(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError);
    | AnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | AnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | AnaInjExpectedTypeNotConsistenWithSums(expected_ty) =>
      let ind1 = expected_type_consistent_with_sums_indicator;
      let ind2 = got_ty_indicator(expected_ty);
      (ind1, ind2, TypeInconsistency);
    | AnaInjBadTag(got_tag) =>
      let ind1 = expected_member_of_sumtype_indicator;
      let ind2 = got_tag_indicator(got_tag);
      (ind1, ind2, TypeInconsistency);
    | AnaInjExpectedBody(expected_ty) =>
      let ind1 = expected_injection_body_indicator(expected_ty);
      let ind2 = got_nothing_indicator;
      (ind1, ind2, TypeInconsistency);
    | AnaInjUnexpectedBody =>
      let ind1 = expected_nothing_indicator;
      let ind2 = got_something_indicator;
      (ind1, ind2, TypeInconsistency);
    | Synthesized(ty) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK);
    | SynInvalid =>
      let ind1 = expected_any_indicator;
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | SynFree =>
      let ind1 = expected_any_indicator;
      let ind2 = got_free_indicator;
      (ind1, ind2, BindingError);
    | SynKeyword(_keyword) =>
      let ind1 = expected_any_indicator;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | SynErrorArrow(expected_ty, got_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 = got_inconsistent_matched_indicator(got_ty, expected_ty);
      (ind1, ind2, TypeInconsistency);
    | SynMatchingArrow(syn_ty, matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        switch (syn_ty) {
        | HTyp.Hole =>
          got_indicator(
            "Got type ▶ matched to",
            matched_ty_bar(syn_ty, matched_ty),
          )
        | _ => got_indicator("Got", typebar(syn_ty))
        };
      (ind1, ind2, OK);
    | SynKeywordArrow(matched_ty, _k) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a keyword ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynInvalidArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got invalid text ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynFreeArrow(matched_ty) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_indicator(
          "Got a free variable ▶ matched to",
          matched_ty_bar(HTyp.Hole, matched_ty),
        );
      (ind1, ind2, BindingError);
    | SynBranchClause(join, typed, branch_index) =>
      let (ind1, ind2, err_state_b) = get_indicator_info(typed);
      let ind1 =
        switch (join) {
        | NoBranches => ind1
        | InconsistentBranchTys(rule_types, path_to_case) =>
          expected_inconsistent_branches_indicator(
            rule_types,
            path_to_case,
            branch_index,
          )
        | JoinTy(ty) => expected_ty_indicator_consistent(ty)
        };
      let (ind2, err_state_b) =
        switch (join, typed) {
        | (JoinTy(ty), Synthesized(got_ty)) =>
          switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
          | (true, true) => (got_as_expected_ty_indicator(got_ty), OK)
          | (true, false) => (got_consistent_indicator(got_ty), OK)
          | (false, _) => (
              got_inconsistent_indicator(got_ty),
              TypeInconsistency,
            )
          }
        | (InconsistentBranchTys(_), _) => (ind2, TypeInconsistency)
        | _ => (ind2, err_state_b)
        };
      (ind1, ind2, err_state_b);
    | SynInconsistentBranches(rule_types, path_to_case) =>
      let ind1 = expected_any_indicator;
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency);
    | SynInconsistentBranchesArrow(rule_types, path_to_case) =>
      let ind1 = expected_msg_indicator("function type");
      let ind2 =
        got_inconsistent_branches_indicator(rule_types, path_to_case);
      (ind1, ind2, TypeInconsistency);
    | OnType =>
      let ind1 = expected_a_type_indicator;
      let ind2 = got_a_type_indicator;
      (ind1, ind2, OK);
    | PatAnalyzed(ty) =>
      let ind1 = expected_ty_indicator_pat(ty);
      let ind2 = got_indicator("Got", special_msg_bar("as expected"));
      (ind1, ind2, OK);
    | PatAnaTypeInconsistent(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_inconsistent_indicator(got_ty);
      (ind1, ind2, TypeInconsistency);
    | PatAnaWrongLength(expected_len, got_len, _expected_ty) =>
      let expected_msg = string_of_int(expected_len) ++ "-tuple";
      let ind1 =
        expected_indicator(
          "Expecting a pattern of form",
          special_msg_bar(expected_msg),
        );
      let got_msg = string_of_int(got_len) ++ "-tuple";
      let ind2 =
        got_indicator(
          "Got tuple of the wrong length",
          special_msg_bar(got_msg),
        );
      (ind1, ind2, TypeInconsistency);
    | PatAnaInvalid(expected_ty) =>
      let ind1 = expected_ty_indicator(expected_ty);
      let ind2 = got_invalid_indicator;
      (ind1, ind2, BindingError);
    | PatAnaSubsumed(expected_ty, got_ty) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 =
        HTyp.eq(expected_ty, got_ty)
          ? got_as_expected_ty_indicator(got_ty)
          : got_consistent_indicator(got_ty);
      (ind1, ind2, OK);
    | PatAnaKeyword(expected_ty, _keyword) =>
      let ind1 = expected_ty_indicator_pat(expected_ty);
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    | PatSynthesized(ty) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_ty_indicator(ty);
      (ind1, ind2, OK);
    | PatSynKeyword(_keyword) =>
      let ind1 = expected_any_indicator_pat;
      let ind2 = got_keyword_indicator;
      (ind1, ind2, BindingError);
    // | AnaInjExpectedTypeNotConsistenWithSums(got_ty) =>
    //   let ind1 = expected_type_consistent_with_sums_indicator;
    //   let ind2 = got_ty_indicator(got_ty);
    //   (ind1, ind2, TypeInconsistency);
    // | AnaInjBadTag(got_tag) =>
    //   let ind1 = expected_member_of_sumtype_indicator;
    //   let ind2 = got_tag_indicator(got_tag);
    //   (ind1, ind2, TypeInconsistency);
    // | AnaInjExpectedBody(expected_ty) =>
    //   let ind1 = expected_member_of_sumtype_indicator;
    //   let ind2 = got_tag_indicator(got_tag);
    //   (ind1, ind2, TypeInconsistency);
    // | AnaInjUnexpectedBody =>
    //   let ind1 = expected_injection_no_body_indicator;
    //   let ind2 = got_something_indicator;
    //   (ind1, ind2, TypeInconsistency);

    | PatAnaInjExpectedTypeNotConsistentWithSums(expected_ty) =>
      let ind1 = expected_type_consistent_with_sums_indicator;
      let ind2 = got_ty_indicator(expected_ty);
      (ind1, ind2, TypeInconsistency);
    | PatAnaInjBadTag(got_tag) =>
      let ind1 = expected_member_of_sumtype_indicator;
      let ind2 = got_tag_indicator(got_tag);
      (ind1, ind2, TypeInconsistency);
    | PatAnaInjExpectedBody(expected_ty) =>
      let ind1 = expected_injection_body_indicator_pat(expected_ty);
      let ind2 = got_nothing_indicator;
      (ind1, ind2, TypeInconsistency);
    | PatAnaInjUnexpectedBody =>
      let ind1 = expected_nothing_indicator;
      let ind2 = got_something_indicator;
      (ind1, ind2, TypeInconsistency);
    | OnTag =>
      let ind1 = expected_a_tag_indicator;
      let ind2 = got_a_tag_indicator;
      (ind1, ind2, OK);
    | OnTagHole =>
      let ind1 = expected_a_tag_indicator;
      let ind2 = got_a_tag_hole_indicator;
      (ind1, ind2, OK);
    | OnInvalidTag(tag) =>
      let ind1 = expected_a_tag_indicator;
      let ind2 = got_invalid_tag_indicator(tag);
      (ind1, ind2, TypeInconsistency);
    | OnUnknownTag(tag) =>
      let ind1 = expected_a_known_tag_indicator;
      let ind2 = got_unknown_tag_indicator(tag);
      (ind1, ind2, TypeInconsistency);
    | OnDuplicateTag(tag) =>
      let ind1 = expected_a_unique_tag_indicator;
      let ind2 = got_duplicate_tag_indicator(tag);
      (ind1, ind2, TypeInconsistency);
    | OnSumBody =>
      let ind1 = expected_a_sum_body_indicator;
      let ind2 = got_a_sum_type_body_indicator;
      (ind1, ind2, OK);
    | OnLine =>
      /* TODO */
      let ind1 = expected_a_line_indicator;
      let ind2 = got_a_line_indicator;
      (ind1, ind2, OK);
    | OnRule =>
      /* TODO */
      let ind1 = expected_a_rule_indicator;
      let ind2 = got_a_rule_indicator;
      (ind1, ind2, OK);
    };

  let (ind1, ind2, err_state_b) = get_indicator_info(ci.typed);

  // this determines the color
  let cls_of_err_state_b =
    switch (err_state_b) {
    | TypeInconsistency => "cursor-TypeInconsistency"
    | BindingError => "cursor-BindingError"
    | OK => "cursor-OK"
    };

  Node.div(
    [Attr.classes(["cursor-inspector-outer"])],
    [
      Node.div(
        [Attr.classes(["panel", "cursor-inspector", cls_of_err_state_b])],
        [ind1, ind2],
      ),
    ],
  );
};

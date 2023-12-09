open Haz3lcore;
open DHDoc_Exp;
open EvaluatorStep;
open Transition;
module Doc = Pretty.Doc;

let mk =
    (
      ~settings: Settings.Evaluation.t,
      ~enforce_inline: bool,
      ~selected_hole_instance: option(HoleInstance.t),
      ~previous_step: option(EvalObj.t), // The step that will be displayed above this one
      ~hidden_steps: list(EvalObj.t), // The hidden steps between the above and the current one
      ~chosen_step: option(EvalObj.t), // The step that will be taken next
      ~next_steps: list(EvalObj.t),
      ~env: ClosureEnvironment.t,
      d: DHExp.t,
    )
    : DHDoc.t => {
  let precedence = precedence(~show_casts=settings.show_casts);
  let rec go =
          (
            d: DHExp.t,
            env: ClosureEnvironment.t,
            enforce_inline: bool,
            previous_step: option(EvalObj.t), // The step that will be displayed above this one
            hidden_steps: list(EvalObj.t), // The hidden steps between the above and the current one
            chosen_step: option(EvalObj.t), // The step that will be taken next
            next_steps: list((EvalObj.t, EvalObj.t)),
          ) // The options for the next step, if they haven't been chosen yet
          : DHDoc.t => {
    open Doc;
    let go' = (~env=env, ~enforce_inline=enforce_inline, d, ctx) =>
      go(
        d,
        env,
        enforce_inline,
        Option.join(Option.map(EvalObj.unwrap(_, ctx), previous_step)),
        List.filter_map(EvalObj.unwrap(_, ctx), hidden_steps),
        Option.join(Option.map(EvalObj.unwrap(_, ctx), chosen_step)),
        List.filter_map(
          ((x, y)) =>
            switch (EvalObj.unwrap(x, ctx)) {
            | None => None
            | Some(x') => Some((x', y))
            },
          next_steps,
        ),
      );
    let parenthesize = (b, doc) =>
      if (b) {
        hcats([
          DHDoc_common.Delim.open_Parenthesized,
          doc |> DHDoc_common.pad_child(~enforce_inline),
          DHDoc_common.Delim.close_Parenthesized,
        ]);
      } else {
        doc(~enforce_inline);
      };
    let go_case_rule =
        (consistent: bool, rule_idx: int, Rule(dp, dclause): DHExp.rule)
        : DHDoc.t => {
      let kind: EvalCtx.cls =
        if (consistent) {
          ConsistentCaseRule(rule_idx);
        } else {
          InconsistentBranchesRule(rule_idx);
        };
      let hidden_clause = annot(DHAnnot.Collapsed, text(Unicode.ellipsis));
      let clause_doc =
        settings.show_case_clauses
          ? choices([
              hcats([space(), go'(~enforce_inline=true, dclause, kind)]),
              hcats([
                linebreak(),
                indent_and_align(go'(~enforce_inline=false, dclause, kind)),
              ]),
            ])
          : hcat(space(), hidden_clause);
      hcats([
        DHDoc_common.Delim.bar_Rule,
        DHDoc_Pat.mk(dp)
        |> DHDoc_common.pad_child(
             ~inline_padding=(space(), space()),
             ~enforce_inline=false,
           ),
        DHDoc_common.Delim.arrow_Rule,
        clause_doc,
      ]);
    };
    let go_case = (dscrut, drs, consistent) =>
      if (enforce_inline) {
        fail();
      } else {
        let kind: EvalCtx.cls =
          if (consistent) {ConsistentCase} else {InconsistentBranches};
        let scrut_doc =
          choices([
            hcats([space(), go'(~enforce_inline=true, dscrut, kind)]),
            hcats([
              linebreak(),
              indent_and_align(go'(~enforce_inline=false, dscrut, kind)),
            ]),
          ]);
        vseps(
          List.concat([
            [hcat(DHDoc_common.Delim.open_Case, scrut_doc)],
            drs |> List.mapi(go_case_rule(consistent)),
            [DHDoc_common.Delim.close_Case],
          ]),
        );
      };
    let go_formattable = (~enforce_inline) => go'(~enforce_inline);
    let mk_left_associative_operands = (precedence_op, (d1, l), (d2, r)) => (
      go_formattable(d1, l) |> parenthesize(precedence(d1) > precedence_op),
      go_formattable(d2, r) |> parenthesize(precedence(d2) >= precedence_op),
    );
    let mk_right_associative_operands = (precedence_op, (d1, l), (d2, r)) => (
      go_formattable(d1, l) |> parenthesize(precedence(d1) >= precedence_op),
      go_formattable(d2, r) |> parenthesize(precedence(d2) > precedence_op),
    );
    let doc = {
      switch (d) {
      /* Now any of the postprocess checking is not done since most of
         the time the result is partial evaluated and those conditions
         cannot be met. */
      | Closure(env', d') => go'(d', Closure, ~env=env')
      | Filter({pat, act}, d') =>
        if (settings.show_filters) {
          let keyword =
            switch (act) {
            | Step => "step"
            | Eval => "skip"
            };
          let flt_doc = go_formattable(pat, FilterPattern);
          vseps([
            hcats([
              DHDoc_common.Delim.mk(keyword),
              flt_doc
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline=false,
                 ),
              DHDoc_common.Delim.mk("in"),
            ]),
            go'(d', Filter),
          ]);
        } else {
          go'(d', Filter);
        }

      /* Hole expressions must appear within a closure in
         the postprocessed result */
      | EmptyHole(u, i) =>
        let selected =
          switch (selected_hole_instance) {
          | None => false
          | Some((u', i')) => u == u' && i == i'
          };
        DHDoc_common.mk_EmptyHole(~selected, (u, i));
      | NonEmptyHole(reason, u, i, d') =>
        go'(d', NonEmptyHole)
        |> annot(DHAnnot.NonEmptyHole(reason, (u, i)))
      | ExpandingKeyword(u, i, k) =>
        DHDoc_common.mk_ExpandingKeyword((u, i), k)
      | FreeVar(u, i, x) =>
        text(x) |> annot(DHAnnot.VarHole(Free, (u, i)))
      | InvalidText(u, i, t) => DHDoc_common.mk_InvalidText(t, (u, i))
      | InconsistentBranches(u, i, Case(dscrut, drs, _)) =>
        go_case(dscrut, drs, false)
        |> annot(DHAnnot.InconsistentBranches((u, i)))

      | BoundVar(x) =>
        if (settings.substitution) {
          switch (ClosureEnvironment.lookup(env, x)) {
          | None => text(x)
          | Some(d') => go'(~env=ClosureEnvironment.empty, d', BoundVar)
          };
        } else {
          text(x);
        }
      | Constructor(name) => DHDoc_common.mk_ConstructorLit(name)
      | BoolLit(b) => DHDoc_common.mk_BoolLit(b)
      | IntLit(n) => DHDoc_common.mk_IntLit(n)
      | FloatLit(f) => DHDoc_common.mk_FloatLit(f)
      | StringLit(s) => DHDoc_common.mk_StringLit(s)
      | Test(_, d) => DHDoc_common.mk_Test(go'(d, Test))
      | Sequence(d1, d2) =>
        let (doc1, doc2) = (go'(d1, Sequence1), go'(d2, Sequence2));
        DHDoc_common.mk_Sequence(doc1, doc2);
      | ListLit(_, _, _, d_list) =>
        let ol = d_list |> List.mapi((i, d) => go'(d, ListLit(i)));
        DHDoc_common.mk_ListLit(ol);
      | Ap(d1, d2) =>
        let (doc1, doc2) = (
          go_formattable(d1, Ap1)
          |> parenthesize(precedence(d1) > DHDoc_common.precedence_Ap),
          go'(d2, Ap2),
        );
        DHDoc_common.mk_Ap(doc1, doc2);
      | ApBuiltin(ident, args) =>
        switch (args) {
        | [hd, ...tl] =>
          let d' = List.fold_left((d1, d2) => DHExp.Ap(d1, d2), hd, tl);
          let (doc1, doc2) =
            mk_left_associative_operands(
              DHDoc_common.precedence_Ap,
              (BoundVar(ident), Ap1),
              (d', Ap2),
            );
          DHDoc_common.mk_Ap(doc1, doc2);
        | [] => text(ident)
        }
      | BinIntOp(op, d1, d2) =>
        // TODO assumes all bin int ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_int_op(op),
            (d1, BinIntOp1),
            (d2, BinIntOp2),
          );
        hseps([doc1, mk_bin_int_op(op), doc2]);
      | BinFloatOp(op, d1, d2) =>
        // TODO assumes all bin float ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_float_op(op),
            (d1, BinFloatOp1),
            (d2, BinFloatOp2),
          );
        hseps([doc1, mk_bin_float_op(op), doc2]);
      | BinStringOp(op, d1, d2) =>
        // TODO assumes all bin string ops are left associative
        let (doc1, doc2) =
          mk_left_associative_operands(
            precedence_bin_string_op(op),
            (d1, BinStringOp1),
            (d2, BinStringOp2),
          );
        hseps([doc1, mk_bin_string_op(op), doc2]);
      | Cons(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Cons,
            (d1, Cons1),
            (d2, Cons2),
          );
        DHDoc_common.mk_Cons(doc1, doc2);
      | ListConcat(d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            DHDoc_common.precedence_Plus,
            (d1, ListConcat1),
            (d2, ListConcat2),
          );
        DHDoc_common.mk_ListConcat(doc1, doc2);
      | BinBoolOp(op, d1, d2) =>
        let (doc1, doc2) =
          mk_right_associative_operands(
            precedence_bin_bool_op(op),
            (d1, BinBoolOp1),
            (d2, BinBoolOp2),
          );
        hseps([doc1, mk_bin_bool_op(op), doc2]);
      | Tuple([]) => DHDoc_common.Delim.triv
      | Tuple(ds) =>
        DHDoc_common.mk_Tuple(ds |> List.mapi((i, d) => go'(d, Tuple(i))))
      | Prj(d, n) => DHDoc_common.mk_Prj(go'(d, Prj), n)
      | ConsistentCase(Case(dscrut, drs, _)) => go_case(dscrut, drs, true)
      | Cast(d, _, ty) when settings.show_casts =>
        // TODO[Matt]: Roll multiple casts into one cast
        let doc = go'(d, Cast);
        Doc.(
          hcat(
            doc,
            annot(
              DHAnnot.CastDecoration,
              DHDoc_Typ.mk(~enforce_inline=true, ty),
            ),
          )
        );
      | Cast(d, _, _) =>
        let doc = go'(d, Cast);
        doc;
      | Let(dp, ddef, dbody) =>
        if (enforce_inline) {
          fail();
        } else {
          let def_doc =
            go_formattable(
              ~env=
                ClosureEnvironment.without_keys(DHPat.bound_vars(dp), env),
              ddef,
              Let1,
            );
          vseps([
            hcats([
              DHDoc_common.Delim.mk("let"),
              DHDoc_Pat.mk(dp)
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline,
                 ),
              DHDoc_common.Delim.mk("="),
              def_doc
              |> DHDoc_common.pad_child(
                   ~inline_padding=(space(), space()),
                   ~enforce_inline=false,
                 ),
              DHDoc_common.Delim.mk("in"),
            ]),
            go'(~enforce_inline=false, dbody, Let2),
          ]);
        }
      | FailedCast(Cast(d, ty1, ty2), ty2', ty3) when Typ.eq(ty2, ty2') =>
        let d_doc = go'(d, FailedCastCast);
        let cast_decoration =
          hcats([
            DHDoc_common.Delim.open_FailedCast,
            hseps([
              DHDoc_Typ.mk(~enforce_inline=true, ty1),
              DHDoc_common.Delim.arrow_FailedCast,
              DHDoc_Typ.mk(~enforce_inline=true, ty3),
            ]),
            DHDoc_common.Delim.close_FailedCast,
          ])
          |> annot(DHAnnot.FailedCastDecoration);
        hcats([d_doc, cast_decoration]);
      | FailedCast(_d, _ty1, _ty2) =>
        failwith("unexpected FailedCast without inner cast")
      | InvalidOperation(d, err) =>
        let d_doc = go'(d, InvalidOperation);
        let decoration =
          Doc.text(InvalidOperationError.err_msg(err))
          |> annot(DHAnnot.OperationError(err));
        hcats([d_doc, decoration]);

      | Fun(dp, ty, dbody, s) =>
        if (settings.show_fn_bodies) {
          let body_doc =
            go_formattable(
              dbody,
              ~env=
                ClosureEnvironment.without_keys(DHPat.bound_vars(dp), env),
              Fun,
            );
          hcats([
            DHDoc_common.Delim.sym_Fun,
            DHDoc_Pat.mk(dp)
            |> DHDoc_common.pad_child(
                 ~inline_padding=(space(), space()),
                 ~enforce_inline,
               ),
            DHDoc_common.Delim.colon_Fun,
            space(),
            DHDoc_Typ.mk(~enforce_inline=true, ty),
            space(),
            DHDoc_common.Delim.open_Fun,
            body_doc |> DHDoc_common.pad_child(~enforce_inline),
            DHDoc_common.Delim.close_Fun,
          ]);
        } else {
          switch (s) {
          | None => annot(DHAnnot.Collapsed, text("<anon fn>"))
          | Some(name) => annot(DHAnnot.Collapsed, text("<" ++ name ++ ">"))
          };
        }
      | FixF(x, ty, dbody) =>
        if (settings.show_fn_bodies) {
          let doc_body =
            go_formattable(
              dbody,
              ~env=ClosureEnvironment.without_keys([x], env),
              FixF,
            );
          hcats([
            DHDoc_common.Delim.fix_FixF,
            space(),
            text(x),
            DHDoc_common.Delim.colon_FixF,
            DHDoc_Typ.mk(~enforce_inline=true, ty),
            DHDoc_common.Delim.open_FixF,
            doc_body |> DHDoc_common.pad_child(~enforce_inline),
            DHDoc_common.Delim.close_FixF,
          ]);
        } else {
          annot(DHAnnot.Collapsed, text("<fn>"));
        }
      };
    };
    let steppable =
      next_steps
      |> List.find_opt(((step, _)) => EvalObj.get_ctx(step) == Mark);
    let stepped =
      chosen_step
      |> Option.map(x => EvalObj.get_ctx(x) == Mark)
      |> Option.value(~default=false);
    let substitution =
      hidden_steps
      |> List.find_opt(step =>
           EvalObj.get_kind(step) == VarLookup
           && EvalObj.get_ctx(step) == Mark
         );
    let doc =
      switch (substitution) {
      | Some(eo) =>
        hcats([
          go'(~env=ClosureEnvironment.empty, eo.undo, BoundVar)
          |> annot(DHAnnot.Stepped),
          doc,
        ])
      | None => doc
      };
    let doc =
      if (stepped) {
        annot(DHAnnot.Stepped, doc);
      } else {
        switch (steppable) {
        | Some((_, full)) => annot(DHAnnot.Steppable(full), doc)
        | None => doc
        };
      };
    doc;
  };
  go(
    d,
    env,
    enforce_inline,
    previous_step,
    hidden_steps,
    chosen_step,
    List.map(x => (x, x), next_steps),
  );
};

open Sexplib.Std;
open OptUtil.Syntax;

[@deriving sexp]
type slice =
  | ExpBlock(ZExp.zblock)
  | ExpLine(ZExp.zline)
  | ExpSeq(ZExp.zopseq)
  | ExpOperand(ZExp.zoperand)
  | ExpOperator(ZExp.zoperator)
  | ExpRules(ZExp.zrules)
  | ExpRule(ZExp.zrule)
  | PatSeq(ZPat.zopseq)
  | PatOperand(ZPat.zoperand)
  | PatOperator(ZPat.zoperator)
  | TypSeq(ZTyp.zopseq)
  | TypOperand(ZTyp.zoperand)
  | TypOperator(ZTyp.zoperator);

[@deriving sexp]
type slice_info = {
  slice,
  ty_e: option(HTyp.t),
  ty_a: option(HTyp.t),
  ctx: Contexts.t,
};

let mk_si =
    (~ctx, ~ty_e: option(HTyp.t), ~ty_a: option(HTyp.t), ~slice: slice)
    : slice_info => {
  ctx,
  slice,
  ty_e,
  ty_a,
};

[@deriving sexp]
type t = list(slice_info);

let synthetic = (~ctx: Contexts.t, slice: slice): option(HTyp.t) =>
  switch (slice) {
  | ExpBlock(zblock) => zblock |> ZExp.erase |> Statics_Exp.syn_block(ctx)
  | ExpLine(CursorL(_, ExpLine(opseq))) =>
    Statics_Exp.syn_opseq(ctx, opseq)
  | ExpLine(_) => None
  | ExpSeq(zopseq) =>
    zopseq
    |> ZExp.erase_zopseq
    |> UHExp.set_err_status_opseq(NotInHole)
    |> Statics_Exp.syn_opseq(ctx)
  | ExpOperand(zoperand) =>
    zoperand
    |> ZExp.erase_zoperand
    |> UHExp.set_err_status_operand(NotInHole)
    |> Statics_Exp.syn_operand(ctx)
  | ExpOperator(_zoperator) => None // TODO(andrew)
  | ExpRules(zrules) =>
    // last arg was scrut_ty  but not sure that makes sense...
    Statics_Exp.syn_rules(ctx, ZExp.erase_zrules(zrules), HTyp.Hole)
  | ExpRule(zrule) =>
    Statics_Exp.syn_rule(ctx, ZExp.erase_zrule(zrule), HTyp.Hole)
  | PatSeq(zopseq) =>
    zopseq
    |> ZPat.erase_zopseq
    |> UHPat.set_err_status_opseq(NotInHole)
    |> Statics_Pat.syn_opseq(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperand(zoperand) =>
    zoperand
    |> ZPat.erase_zoperand
    |> UHPat.set_err_status_operand(NotInHole)
    |> Statics_Pat.syn_operand(ctx)
    |> Option.map(((ty, _)) => ty)
  | PatOperator(_zoperator) => None // TODO(andrew)
  | TypSeq(_)
  | TypOperand(_)
  | TypOperator(_) => None
  };

let expected_ty_from_ty_mode: Statics.type_mode => HTyp.t =
  fun
  | Ana(ty) => ty
  | Syn => HTyp.Hole;

let analytic =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), slice: slice): option(HTyp.t) =>
  switch (slice) {
  | ExpOperand(CursorE(_))
  | ExpOperand(ApPaletteZ(_))
  | PatOperand(CursorP(_))
  | ExpLine(CursorL(_))
  | ExpRule(CursorR(_)) => None // these have no children
  | ExpBlock((_, _, suffix)) =>
    // last line inherits type (could be last ExpLine)
    suffix == [] ? ty_e : None
  | ExpLine(LetLineZE(p, _)) =>
    switch (Statics_Pat.syn(ctx, p)) {
    | Some((ty, _)) => Some(ty)
    | None => Some(HTyp.Hole)
    }
  | ExpLine(LetLineZP(_)) =>
    // could incorporate def type if any
    None
  | ExpLine(ExpLineZ(_)) => ty_e
  | ExpOperand(ParenthesizedZ(_))
  | PatOperand(ParenthesizedZ(_)) => ty_e
  | ExpOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | PatOperand(InjZ(_, side, _)) =>
    let* ty = ty_e;
    let+ (ty1, ty2) = HTyp.matched_sum(ty);
    InjSide.pick(side, ty1, ty2);
  | ExpOperand(LamZE(_)) =>
    let* ty_e' = ty_e;
    let+ (_, ty_body) = HTyp.matched_arrow(ty_e');
    ty_body;
  | PatOperand(TypeAnnZP(_, _, ann)) => Some(UHTyp.expand(ann))
  | PatOperand(TypeAnnZA(_, _, _)) => None
  | ExpOperand(CaseZE(_)) =>
    // could incoporate joint pattern type here if any...
    None
  | ExpOperand(LamZP(_)) => None
  | ExpOperand(CaseZR(_, scrut, _)) =>
    // let's pretend rules have type ty_scrut => ty_expected
    let* ty_e' = ty_e;
    let+ ty_scrut = Statics_Exp.syn(ctx, scrut);
    HTyp.Arrow(ty_scrut, ty_e');
  | ExpRules(_) => ty_e
  | ExpRule(RuleZP(_)) =>
    switch (ty_e) {
    | Some(Arrow(ty_scrut, _)) => Some(ty_scrut)
    | x => x
    }
  | ExpRule(RuleZE(_)) =>
    switch (ty_e) {
    | Some(Arrow(_, ty_top)) => Some(ty_top)
    | x => x
    }
  | ExpSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZExp.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    switch (ty_e) {
    | Some(ty_e) =>
      let+ ty_mode_operand =
        Statics_Exp.ana_nth_type_mode(ctx, operand_index, opseq, ty_e);
      expected_ty_from_ty_mode(ty_mode_operand);
    | None =>
      // TODO: not sure this case should be necessary...
      let+ ty_mode_operand =
        Statics_Exp.syn_nth_type_mode(ctx, operand_index, opseq);
      expected_ty_from_ty_mode(ty_mode_operand);
    };
  | PatSeq(ZOpSeq(_, ZOperand(_, (prefix, _))) as zopseq) =>
    let opseq = ZPat.erase_zopseq(zopseq);
    let operand_index = Seq.length_of_affix(prefix);
    let* ty_e' = ty_e;
    let+ ty_mode_operand =
      Statics_Pat.ana_nth_type_mode(ctx, operand_index, opseq, ty_e');
    expected_ty_from_ty_mode(ty_mode_operand);
  | ExpSeq(ZOpSeq(_, ZOperator(_zop, _))) =>
    //TODO(andrew): FIX adapt syn/ana_nth_type_mode to return operator types
    None
  | PatSeq(ZOpSeq(_, ZOperator(_))) =>
    // TODO(andrew): FIX adapt syn/ana_nth_type_mode to return operator types
    None
  | TypSeq(_)
  | TypOperand(_) => None
  | ExpOperator(_)
  | PatOperator(_)
  | TypOperator(_) => None
  };

let get_ctx =
    (~ctx: Contexts.t, ~ty_e: option(HTyp.t), slice: slice): Contexts.t => {
  // TODO(andrew): does let body ctx get incorporated somewhere?
  //let body_ctx = Statics_Exp.extend_let_body_ctx(ctx, p, def);
  switch (slice) {
  | ExpBlock((prefix, _zline, _suffix)) =>
    //print_endline("get_ctx: calculating context for zline");
    switch (Statics_Exp.syn_lines(ctx, prefix)) {
    | None =>
      //P.p("new ctx (none): %s\n", Contexts.sexp_of_t(ctx));
      ctx
    | Some(ctx) =>
      //P.p("new ctx (some): %s\n", Contexts.sexp_of_t(ctx));
      ctx
    }
  | ExpLine(LetLineZE(p, zblock)) =>
    zblock |> ZExp.erase |> Statics_Exp.extend_let_def_ctx(ctx, p)
  | ExpOperand(LamZE(_, p, _)) =>
    switch (
      {
        let* ty_e' = ty_e;
        let* (ty_p_given, _) = HTyp.matched_arrow(ty_e');
        Statics_Pat.ana(ctx, p, ty_p_given);
      }
    ) {
    | None => ctx
    | Some(ctx) => ctx
    }
  | _ => ctx
  };
};

let get_zchild_slice = (slice: slice): list(slice) => {
  switch (slice) {
  | ExpBlock((_, zline, _)) => [ExpLine(zline)]
  | ExpLine(CursorL(_)) => []
  | ExpLine(ExpLineZ(zopseq)) => [ExpSeq(zopseq)]
  | ExpLine(LetLineZE(_, zblock)) => [ExpBlock(zblock)]
  | ExpLine(LetLineZP(zopseq, _)) => [PatSeq(zopseq)]
  | ExpSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [ExpOperand(zoperand)]
  | ExpSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [ExpOperator(zoperator)]
  | ExpOperator(_zoperator) => [] // TODO(andrew)
  | ExpOperand(CursorE(_)) => []
  | ExpOperand(ApPaletteZ(_)) => []
  | ExpOperand(ParenthesizedZ(zblock))
  | ExpOperand(LamZE(_, _, zblock))
  | ExpOperand(InjZ(_, _, zblock))
  | ExpOperand(CaseZE(_, zblock, _)) => [ExpBlock(zblock)]
  | ExpOperand(LamZP(_, zopseq, _)) => [PatSeq(zopseq)]
  | ExpOperand(CaseZR(_, _, zrules)) => [ExpRules(zrules)]
  | ExpRules((_, zrule, _)) => [ExpRule(zrule)]
  | ExpRule(CursorR(_)) => []
  | ExpRule(RuleZP(zopseq, _)) => [PatSeq(zopseq)]
  | ExpRule(RuleZE(_, zblock)) => [ExpBlock(zblock)]
  | PatSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [PatOperand(zoperand)]
  | PatSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [PatOperator(zoperator)]
  | PatOperator(_zop) => [] // TODO(andrew)
  | PatOperand(CursorP(_)) => []
  | PatOperand(ParenthesizedZ(zopseq))
  | PatOperand(InjZ(_, _, zopseq)) => [PatSeq(zopseq)]
  | PatOperand(TypeAnnZA(_, _, zopseq)) => [TypSeq(zopseq)]
  | PatOperand(TypeAnnZP(_, zoperand, _)) => [PatOperand(zoperand)]
  | TypSeq(ZOpSeq(_, ZOperand(zoperand, _))) => [TypOperand(zoperand)]
  | TypSeq(ZOpSeq(_, ZOperator(zoperator, _))) => [TypOperator(zoperator)]
  | TypOperand(_tyoperand) => [] // TODO(andrew)
  | TypOperator(_tyoperator) => [] // TODO(andrew)
  };
};

// *****************************************************************

let rec mk_frame =
        (slice: slice, ~ctx: Contexts.t, ~ty_e: option(HTyp.t))
        : list(slice_info) => {
  let head = mk_si(~ctx, ~slice, ~ty_e, ~ty_a=synthetic(~ctx, slice));
  let tail =
    switch (get_zchild_slice(slice)) {
    | [child_slice] =>
      let ctx_new = get_ctx(~ctx, ~ty_e, slice);
      let ty_e_new = analytic(~ctx=ctx_new, ~ty_e, slice);
      // TODO: doublecheck logic about new_ctx getting used for ty_e_new
      // i.e. make sure we dont sometimes have to use ty_e_new for getting ctx_new
      mk_frame(child_slice, ~ctx=ctx_new, ~ty_e=ty_e_new);
    | _ => []
    };
  [head, ...tail];
};

let mk = (zexp: ZExp.t): t =>
  ExpBlock(zexp)
  |> mk_frame(~ctx=Contexts.empty, ~ty_e=Some(Hole))
  |> List.rev;

// *****************************************************************

let get_cursor_slice = (zexp: ZExp.t): option(slice_info) =>
  switch (mk(zexp)) {
  | [si, ..._] => Some(si)
  | [] => None
  };

let first_exp_operand = si =>
  switch (si.slice) {
  | ExpOperand(zop) => Some(zop)
  | _ => None
  };

let first_exp_seq_zopseq = si =>
  switch (si.slice) {
  | ExpSeq(zopseq) => Some(zopseq)
  | _ => None
  };

let first_exp_seq_ty_e = si =>
  switch (si) {
  | {slice: ExpSeq(_), ty_e, _} => ty_e
  | _ => None
  };

// omcaml 4.10.0 sneak peal:
let rec find_map = (f: 'a => option('b), xs: list('a)): option('b) => {
  switch (xs) {
  | [] => None
  | [x, ...xs'] =>
    switch (f(x)) {
    | None => find_map(f, xs')
    | x => x
    }
  };
};

let pop_exp_operand = frame =>
  switch (frame) {
  | [{slice: ExpOperand(_), _}, ...xs]
  | xs => xs
  };

let get_opParent = (zexp: ZExp.t): option(ZExp.zoperand) =>
  // skip cursor_term if it's an operand
  zexp |> mk |> pop_exp_operand |> find_map(first_exp_operand);

let enclosing_zopseq = (zexp: ZExp.t): option(ZExp.zopseq) =>
  zexp |> mk |> find_map(first_exp_seq_zopseq);

let enclosing_zopseq_expected_ty = (zexp: ZExp.t): option(HTyp.t) =>
  zexp |> mk |> find_map(first_exp_seq_ty_e);

let get_expected_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_e;
};

let get_actual_type_cursor_term = (zexp: ZExp.t): option(HTyp.t) => {
  let* slice = get_cursor_slice(zexp);
  slice.ty_a;
};

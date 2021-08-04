let operator_of_shape = (os: Action.operator_shape): option(UHTyp.operator) =>
  switch (os) {
  | SArrow => Some(Arrow)
  | SComma => Some(Prod)
  | SPlus => Some(Sum)
  | SVBar
  | SMinus
  | STimes
  | SDivide
  | SAnd
  | SOr
  | SLessThan
  | SGreaterThan
  | SEquals
  | SSpace
  | SCons => None
  };

let shape_of_operator = (op: UHTyp.operator): Action.operator_shape =>
  switch (op) {
  | Arrow => SArrow
  | Prod => SComma
  | Sum => SVBar
  };

let sumtyp_operator_of_shape =
    (sos: Action.sumtyp_operator_shape): option(UHTyp.sumtyp_operator) =>
  switch (sos) {
  | SPlus => Some(Plus)
  };

let shape_of_sumtyp_operator =
    (op: UHTyp.sumtyp_operator): Action.sumtyp_operator_shape =>
  switch (op) {
  | Plus => SPlus
  };

let construct_operator =
    (
      operator: UHTyp.operator,
      zoperand: ZTyp.zoperand,
      (prefix, suffix): ZTyp.operand_surround,
    )
    : ZTyp.zopseq => {
  let operand = zoperand |> ZTyp.erase_zoperand;
  let (zoperand, surround) =
    if (ZTyp.is_before_zoperand(zoperand)) {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix));
    } else {
      let zoperand = UHTyp.Hole |> ZTyp.place_before_operand;
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix));
    };
  ZTyp.mk_ZOpSeq(ZOperand(zoperand, surround));
};

let construct_zsumtyp_operator =
    (
      operator: UHTyp.sumtyp_operator,
      zoperand: ZTyp.zsumtyp_operand,
      (prefix, suffix): ZTyp.sumtyp_operand_surround,
    )
    : ZTyp.zsumtyp => {
  let operand = zoperand |> ZTyp.erase_zsumtyp_operand;
  let (zoperand, surround) =
    if (ZTyp.is_before_zsumtyp_operand(zoperand)) {
      let zoperand = ZTyp.ConstTagZ(UHTag.TagHole(0) |> ZTag.place_before);
      let new_suffix = Seq.A(operator, S(operand, suffix));
      (zoperand, (prefix, new_suffix));
    } else {
      let zoperand = ZTyp.ConstTagZ(UHTag.TagHole(0) |> ZTag.place_before);
      let new_prefix = Seq.A(operator, S(operand, prefix));
      (zoperand, (new_prefix, suffix));
    };
  ZTyp.mk_sumtyp_ZOpSeq(ZOperand(zoperand, surround));
};

let rec move =
        (u_gen: MetaVarGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow(path, zty |> ZTyp.erase)) {
    | None => Failed
    | Some(zty) => Succeeded((zty, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.(prev_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), zty)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(next_hole_steps(CursorPath_Typ.holes_z(zty, [])))
    ) {
    | None => Failed
    | Some(steps) =>
      switch (CursorPath_Typ.of_steps(steps, zty |> ZTyp.erase)) {
      | None => Failed
      | Some(path) => move(u_gen, MoveTo(path), zty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left(zty)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, u_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right(zty)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, u_gen))
    }
  | Construct(_)
  | Delete
  | Backspace
  | UpdateApPalette(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  }
and move_zsumtyp =
    (u_gen: MetaVarGen.t, a: Action.t, zsumty: ZTyp.zsumtyp)
    : ActionOutcome.t((ZTyp.zsumtyp, MetaVarGen.t)) =>
  switch (a) {
  | MoveTo(path) =>
    switch (CursorPath_Typ.follow_sumtyp(path, zsumty |> ZTyp.erase_zsumtyp)) {
    | None => Failed
    | Some(zsumty) => Succeeded((zsumty, u_gen))
    }
  | MoveToPrevHole =>
    switch (
      CursorPath_common.prev_hole_steps(
        CursorPath_Typ.holes_zsumtyp(zsumty, []),
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sumtyp(steps, zsumty |> ZTyp.erase_zsumtyp)
      ) {
      | None => Failed
      | Some(path) => move_zsumtyp(u_gen, MoveTo(path), zsumty)
      }
    }
  | MoveToNextHole =>
    switch (
      CursorPath_common.(
        next_hole_steps(CursorPath_Typ.holes_zsumtyp(zsumty, []))
      )
    ) {
    | None => Failed
    | Some(steps) =>
      switch (
        CursorPath_Typ.of_steps_sumtyp(steps, zsumty |> ZTyp.erase_zsumtyp)
      ) {
      | None => Failed
      | Some(path) => move_zsumtyp(u_gen, MoveTo(path), zsumty)
      }
    }
  | MoveLeft =>
    switch (ZTyp.move_cursor_left_zsumtyp(zsumty)) {
    | None => ActionOutcome.CursorEscaped(Before)
    | Some(z) => Succeeded((z, u_gen))
    }
  | MoveRight =>
    switch (ZTyp.move_cursor_right_zsumtyp(zsumty)) {
    | None => ActionOutcome.CursorEscaped(After)
    | Some(z) => Succeeded((z, u_gen))
    }
  | UpdateApPalette(_)
  | Delete
  | Backspace
  | Construct(_)
  | SwapLeft
  | SwapRight
  | SwapUp
  | SwapDown
  | Init =>
    failwith(
      __LOC__
      ++ ": expected movement action, got "
      ++ Sexplib.Sexp.to_string(Action.sexp_of_t(a)),
    )
  };

let rec perform =
        (u_gen: MetaVarGen.t, a: Action.t, zty: ZTyp.t)
        : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  perform_opseq(u_gen, a, zty)
and perform_opseq =
    (
      u_gen: MetaVarGen.t,
      a: Action.t,
      ZOpSeq(skel, zseq) as zopseq: ZTyp.zopseq,
    )
    : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, zopseq)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_opseq(u_gen, Action_common.escape(side), zopseq)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_opseq(
      u_gen,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    )
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ...
   * (for now until we have proper type constructors) */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zoperand = prefix_hd |> ZTyp.place_after_operand;
    let S(_, new_suffix) = suffix;
    Succeeded((
      ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
      u_gen,
    ));

  /* Construction */
  /* construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_opseq(u_gen, MoveRight, zopseq)
  /* ...or construction after movement */
  | (Construct(_) as a, ZOperator((OnOp(side), _), _)) =>
    switch (perform_opseq(u_gen, Action_common.escape(side), zopseq)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zty, u_gen)) => perform(u_gen, a, zty)
    }

  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zoperand(zoperand) =>
    perform_opseq(u_gen, MoveRight, zopseq)

  | (Construct(SOp(os)), ZOperand(CursorT(_) as zoperand, surround)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded((construct_operator(op, zoperand, surround), u_gen))
    }

  /* SwapLeft and SwapRight is handled at block level */

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorT(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorT(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), u_gen));
  | (SwapRight, ZOperand(CursorT(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorT(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_ZOpSeq(new_zseq), u_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_opseq(u_gen, Action_common.escape(side), zopseq)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperand(zoperand, (new_prefix, new_suffix))),
          u_gen,
        ));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_ZOpSeq(ZOperator(zoperator, (new_prefix, new_suffix))),
          u_gen,
        ));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and perform_operand =
    (u_gen: MetaVarGen.t, a: Action.t, zoperand: ZTyp.zoperand)
    : ActionOutcome.t((ZTyp.t, MetaVarGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_) |
        SCommentLine |
        SSumOp(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorT(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorT(cursor, operand))
      when !ZTyp.is_valid_cursor_operand(cursor, operand) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move(u_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* ( _ <|)   ==>   ( _| ) */
  | (Backspace, CursorT(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zoperand
      ? CursorEscaped(Before) : perform_operand(u_gen, MoveLeft, zoperand)
  /* (|> _ )   ==>   ( |_ ) */
  | (Delete, CursorT(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zoperand
      ? CursorEscaped(After) : perform_operand(u_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorT(OnDelim(k, Before), operand)) =>
    perform_operand(u_gen, Backspace, CursorT(OnDelim(k, After), operand))

  | (Backspace, CursorT(OnDelim(_, After), Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), u_gen))

  | (Backspace, CursorT(OnDelim(_, After), Unit | Int | Float | Bool)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_before_operand(Hole)), u_gen))

  /* ( _ )<|  ==>  _| */
  /* (<| _ )  ==>  |_ */
  | (
      Backspace,
      CursorT(OnDelim(k, After), Parenthesized(body) | List(body)),
    ) =>
    let place_cursor = k == 0 ? ZTyp.place_before : ZTyp.place_after;
    Succeeded((body |> place_cursor, u_gen));

  /* { _ }<|  ==>  _| */
  /* {<| _ }  ==>  |_ */
  | (Backspace, CursorT(OnDelim(k, After), Sum(sumty))) =>
    let place_cursor =
      k == 0 ? ZTyp.place_before_sumtyp : ZTyp.place_after_sumtyp;
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(sumty |> place_cursor)), u_gen));

  /* Construction */

  | (Construct(SOp(SSpace)), CursorT(OnDelim(_, After), _)) =>
    perform_operand(u_gen, MoveRight, zoperand)
  | (Construct(_) as a, CursorT(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zoperand(zoperand)
        && !ZTyp.is_after_zoperand(zoperand) =>
    switch (perform_operand(u_gen, Action_common.escape(side), zoperand)) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zty, u_gen)) => perform(u_gen, a, zty)
    }

  | (Construct(SChar("I")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Int)), u_gen))
  | (Construct(SChar("F")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Float)), u_gen))
  | (Construct(SChar("B")), CursorT(_, Hole)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.place_after_operand(Bool)), u_gen))
  | (Construct(SChar(_)), CursorT(_)) => Failed

  | (Construct(SList), CursorT(_)) =>
    Succeeded((ZOpSeq.wrap(ZTyp.ListZ(ZOpSeq.wrap(zoperand))), u_gen))

  | (Construct(SSum), CursorT(_, Hole)) =>
    let ztag = ZTag.place_before(UHTag.TagHole(0));
    let zsumty = ZTyp.mk_sumtyp_ZOpSeq(ZSeq.wrap(ZTyp.ConstTagZ(ztag)));
    Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumty)), u_gen));

  | (Construct(SSum), CursorT(_, _)) => Failed

  | (Construct(SParenthesized), CursorT(_)) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.ParenthesizedZ(ZOpSeq.wrap(zoperand))),
      u_gen,
    ))

  | (Construct(SOp(os)), CursorT(_)) =>
    switch (operator_of_shape(os)) {
    | None => Failed
    | Some(op) =>
      Succeeded((construct_operator(op, zoperand, (E, E)), u_gen))
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorT(_)) => Failed

  /* Zipper Cases */
  | (_, ParenthesizedZ(zbody)) =>
    switch (perform(u_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ParenthesizedZ(zbody)), u_gen))
    }
  | (_, ListZ(zbody)) =>
    switch (perform(u_gen, a, zbody)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zbody, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ListZ(zbody)), u_gen))
    }
  | (_, SumZ(zsumty)) =>
    switch (perform_zsumtyp(u_gen, a, zsumty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zsumty, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.SumZ(zsumty)), u_gen))
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and perform_zsumtyp =
    (
      u_gen: MetaVarGen.t,
      a: Action.t,
      ZOpSeq(skel, zseq) as zsumty: ZTyp.zsumtyp,
    )
    : ActionOutcome.t((ZTyp.zsumtyp, MetaVarGen.t)) =>
  switch (a, zseq) {
  /* Invalid actions at the top level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SListNil | SInj | SCase | SApPalette(_),
      ) |
      SwapUp |
      SwapDown,
      _,
    )
  /* Invalid cursor positions */
  | (_, ZOperator((OnText(_) | OnDelim(_), _), _)) => Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumtyp(u_gen, a, zsumty)

  /* Deletion */

  | (Delete, ZOperator((OnOp(After as side), _), _))
  | (Backspace, ZOperator((OnOp(Before as side), _), _)) =>
    perform_zsumtyp(u_gen, Action_common.escape(side), zsumty)

  /* Delete before operator == Backspace after operator */
  | (Delete, ZOperator((OnOp(Before), op), surround)) =>
    perform_zsumtyp(
      u_gen,
      Backspace,
      ZOpSeq(skel, ZOperator((OnOp(After), op), surround)),
    )
  // TODO:
  // - if there's nothing to the right, delete the "+ ?"
  // - if there's something to the right, delete the "_ +" (whether or not there's something on the left)
  /* ... + [k-2] + [k-1] +<| [k] + ...   ==>   ... + [k-2] + [k-1]| + ... */
  | (Backspace, ZOperator((OnOp(After), _), (prefix, suffix))) =>
    let S(prefix_hd, new_prefix) = prefix;
    let zsumty = prefix_hd |> ZTyp.place_after_sumtyp_operand;
    let S(_, new_suffix) = suffix;
    Succeeded((
      ZTyp.mk_sumtyp_ZOpSeq(ZOperand(zsumty, (new_prefix, new_suffix))),
      u_gen,
    ));

  /* Construction */
  /* construction on operators becomes movement... */
  | (Construct(SOp(SSpace)), ZOperator((OnOp(After), _), _)) =>
    perform_zsumtyp(u_gen, MoveRight, zsumty)
  /* ...or construction after movement */
  | (Construct(_), ZOperator((OnOp(side), _), _)) =>
    switch (perform_zsumtyp(u_gen, Action_common.escape(side), zsumty)) {
    | Failed
    | CursorEscaped(_) => Failed
    | Succeeded((zsumty, u_gen)) => perform_zsumtyp(u_gen, a, zsumty)
    }

  /* Space becomes movement until we have proper type constructors */
  | (Construct(SOp(SSpace)), ZOperand(zoperand, _))
      when ZTyp.is_after_zsumtyp_operand(zoperand) =>
    perform_zsumtyp(u_gen, MoveRight, zsumty)

  | (Construct(SSumOp(sos)), ZOperand(CursorTS(_) as zoperand, surround)) =>
    switch (sumtyp_operator_of_shape(sos)) {
    | None => Failed
    | Some(op) =>
      Succeeded((construct_zsumtyp_operator(op, zoperand, surround), u_gen))
    }

  /* SwapLeft and SwapRight is handled at block level */

  | (SwapLeft, ZOperator(_))
  | (SwapRight, ZOperator(_)) => Failed

  | (SwapLeft, ZOperand(CursorTS(_), (E, _))) => Failed
  | (
      SwapLeft,
      ZOperand(
        CursorTS(_) as zoperand,
        (A(operator, S(operand, new_prefix)), suffix),
      ),
    ) =>
    let new_suffix = Seq.A(operator, S(operand, suffix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumtyp_ZOpSeq(new_zseq), u_gen));
  | (SwapRight, ZOperand(CursorTS(_), (_, E))) => Failed
  | (
      SwapRight,
      ZOperand(
        CursorTS(_) as zoperand,
        (prefix, A(operator, S(operand, new_suffix))),
      ),
    ) =>
    let new_prefix = Seq.A(operator, S(operand, prefix));
    let new_zseq = ZSeq.ZOperand(zoperand, (new_prefix, new_suffix));
    Succeeded((ZTyp.mk_sumtyp_ZOpSeq(new_zseq), u_gen));

  /* Zipper */
  | (_, ZOperand(zoperand, (prefix, suffix))) =>
    switch (perform_zsumtyp_operand(u_gen, a, zoperand)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumtyp(u_gen, Action_common.escape(side), zsumty)
    | Succeeded((ZOpSeq(_, zseq), u_gen)) =>
      switch (zseq) {
      | ZOperand(zoperand, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.affix_affix(inner_prefix, prefix);
        let new_suffix = Seq.affix_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_sumtyp_ZOpSeq(
            ZOperand(zoperand, (new_prefix, new_suffix)),
          ),
          u_gen,
        ));
      | ZOperator(zoperator, (inner_prefix, inner_suffix)) =>
        let new_prefix = Seq.seq_affix(inner_prefix, prefix);
        let new_suffix = Seq.seq_affix(inner_suffix, suffix);
        Succeeded((
          ZTyp.mk_sumtyp_ZOpSeq(
            ZOperator(zoperator, (new_prefix, new_suffix)),
          ),
          u_gen,
        ));
      }
    }
  | (Init, _) => failwith("Init action should not be performed.")
  }
and perform_zsumtyp_operand =
    (u_gen: MetaVarGen.t, a: Action.t, zoperand: ZTyp.zsumtyp_operand)
    : ActionOutcome.t((ZTyp.zsumtyp, MetaVarGen.t)) =>
  switch (a, zoperand) {
  /* Invalid actions at the type level */
  | (
      UpdateApPalette(_) |
      Construct(
        SAnn | SLet | SLine | SLam | SList | SParenthesized | SChar(_) | SOp(_) |
        SListNil |
        SSum |
        SInj |
        SCase |
        SApPalette(_) |
        SCommentLine,
      ) |
      SwapUp |
      SwapDown,
      _,
    ) =>
    Failed

  /* Invalid cursor positions */
  | (_, CursorTS(OnText(_) | OnOp(_), _)) => Failed
  | (_, CursorTS(cursor, operand))
      when !ZTyp.is_valid_cursor_sumtyp_operand(cursor, operand) =>
    Failed

  /* Movement handled at top level */
  | (MoveTo(_) | MoveToPrevHole | MoveToNextHole | MoveLeft | MoveRight, _) =>
    move_zsumtyp(u_gen, a, ZOpSeq.wrap(zoperand))

  /* Backspace and Delete */

  /* _ <|( _ )   ==>   _| ( _ ) */
  /* _ ( _ <|)   ==>   _ ( _| ) */
  /* _ <|   ==>   _| */
  | (Backspace, CursorTS(OnDelim(_, Before), _)) =>
    zoperand |> ZTyp.is_before_zsumtyp_operand
      ? CursorEscaped(Before)
      : perform_zsumtyp_operand(u_gen, MoveLeft, zoperand)
  /* |> _ ( _ )   ==>   |_ ( _ ) */
  /* _ (|> _ )   ==>   _ ( |_ ) */
  /* |> _   ==>   |_ */
  | (Delete, CursorTS(OnDelim(_, After), _)) =>
    zoperand |> ZTyp.is_after_zsumtyp_operand
      ? CursorEscaped(After)
      : perform_zsumtyp_operand(u_gen, MoveRight, zoperand)

  /* Delete before delimiter == Backspace after delimiter */
  | (Delete, CursorTS(OnDelim(k, Before), operand)) =>
    perform_zsumtyp_operand(
      u_gen,
      Backspace,
      CursorTS(OnDelim(k, After), operand),
    )

  /* _ ( _ )<|   ==>   _ | */
  /* _ (<| _ )   ==>   _ | */
  | (Backspace, CursorTS(OnDelim(_, After), ArgTag(tag, _))) =>
    Succeeded((
      ZOpSeq.wrap(ZTyp.place_after_sumtyp_operand(ConstTag(tag))),
      u_gen,
    ))

  | (Backspace, CursorTS(OnDelim(_, After), ConstTag(_))) => Failed

  /* Construction */

  | (Construct(SSumOp(SPlus)), CursorTS(OnDelim(_, After), _)) =>
    perform_zsumtyp_operand(u_gen, MoveRight, zoperand)
  | (Construct(_), CursorTS(OnDelim(_, side), _))
      when
        !ZTyp.is_before_zsumtyp_operand(zoperand)
        && !ZTyp.is_after_zsumtyp_operand(zoperand) =>
    switch (
      perform_zsumtyp_operand(u_gen, Action_common.escape(side), zoperand)
    ) {
    | (Failed | CursorEscaped(_)) as err => err
    | Succeeded((zsumty, u_gen)) => perform_zsumtyp(u_gen, a, zsumty)
    }

  | (Construct(SSumOp(sos)), CursorTS(_)) =>
    switch (sumtyp_operator_of_shape(sos)) {
    | None => Failed
    | Some(sop) =>
      Succeeded((construct_zsumtyp_operator(sop, zoperand, (E, E)), u_gen))
    }

  /* Invalid SwapLeft and SwapRight actions */
  | (SwapLeft | SwapRight, CursorTS(_)) => Failed

  /* Zipper Cases */
  | (_, ConstTagZ(ztag)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumtyp_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ConstTagZ(ztag)), u_gen))
    }
  | (_, ArgTagZT(ztag, ty)) =>
    switch (Action_Tag.perform(u_gen, a, ztag)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumtyp_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((ztag, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZT(ztag, ty)), u_gen))
    }
  | (_, ArgTagZA(tag, zty)) =>
    switch (perform(u_gen, a, zty)) {
    | Failed => Failed
    | CursorEscaped(side) =>
      perform_zsumtyp_operand(u_gen, Action_common.escape(side), zoperand)
    | Succeeded((zty, u_gen)) =>
      Succeeded((ZOpSeq.wrap(ZTyp.ArgTagZA(tag, zty)), u_gen))
    }

  | (Init, _) => failwith("Init action should not be performed.")
  };

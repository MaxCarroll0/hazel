let mk_hook = CursorPath.mk_hook;
let rec of_z = (zty: ZTyp.t): CursorPath.t => of_zopseq(zty)
and of_zopseq = zopseq => CursorPath_common.of_zopseq_(~of_zoperand, zopseq)
and of_zoperand =
  fun
  | CursorT(cursor, _) => ([], cursor)
  | ParenthesizedZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
  | ListZ(zbody) => CursorPath_common.cons'(0, of_z(zbody))
and of_zoperator =
  fun
  | (cursor, _) => ([], cursor);

let rec follow = (path: CursorPath.t, uty: UHTyp.t): option(ZTyp.t) =>
  follow_opseq(path, uty)
and follow_opseq =
    (path: CursorPath.t, opseq: UHTyp.opseq): option(ZTyp.zopseq) =>
  CursorPath_common.follow_opseq_(
    ~follow_operand,
    ~follow_operator,
    path,
    opseq,
  )
and follow_operand =
    ((steps, cursor): CursorPath.t, operand: UHTyp.operand)
    : option(ZTyp.zoperand) =>
  switch (steps) {
  | [] => operand |> ZTyp.place_cursor_operand(cursor)
  | [x, ...xs] =>
    switch (operand) {
    | Hole
    | Unit
    | Int
    | Float
    | Bool => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZTyp.ParenthesizedZ(zbody))
      | _ => None
      }
    | List(body) =>
      switch (x) {
      | 0 =>
        body
        |> follow((xs, cursor))
        |> Option.map(zbody => ZTyp.ListZ(zbody))
      | _ => None
      }
    }
  }
and follow_operator =
    ((steps, cursor): CursorPath.t, operator: UHTyp.operator)
    : option(ZTyp.zoperator) =>
  switch (steps) {
  | [] => operator |> ZTyp.place_cursor_operator(cursor)
  | [_, ..._] => None
  };

let rec of_steps =
        (steps: CursorPath.steps, ~side: Side.t=Before, uty: UHTyp.t)
        : option(CursorPath.t) =>
  of_steps_opseq(steps, ~side, uty)
and of_steps_opseq =
    (steps: CursorPath.steps, ~side: Side.t, opseq: UHTyp.opseq)
    : option(CursorPath.t) =>
  CursorPath_common.of_steps_opseq_(
    ~of_steps_operand,
    ~of_steps_operator,
    steps,
    ~side,
    opseq,
  )
and of_steps_operand =
    (steps: CursorPath.steps, ~side: Side.t, operand: UHTyp.operand)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_operand
      | After => ZTyp.place_after_operand
      };
    Some(of_zoperand(place_cursor(operand)));
  | [x, ...xs] =>
    switch (operand) {
    | Hole
    | Unit
    | Int
    | Float
    | Bool => None
    | Parenthesized(body) =>
      switch (x) {
      | 0 =>
        of_steps(xs, ~side, body)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    | List(body) =>
      switch (x) {
      | 0 =>
        body
        |> of_steps(xs, ~side)
        |> Option.map(path => CursorPath_common.cons'(0, path))
      | _ => None
      }
    }
  }
and of_steps_operator =
    (steps: CursorPath.steps, ~side: Side.t, operator: UHTyp.operator)
    : option(CursorPath.t) =>
  switch (steps) {
  | [_, ..._] => None
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTyp.place_before_operator
      | After => ZTyp.place_after_operator
      };
    switch (place_cursor(operator)) {
    | Some(zty) => Some(of_zoperator(zty))
    | _ => None
    };
  };

let hook = _ => CursorPath.TypHole;
let is_space = _ => false;

let rec hooks =
        (
          uty: UHTyp.t,
          rev_steps: CursorPath.rev_steps,
          hs: CursorPath.hook_list,
        )
        : CursorPath.hook_list =>
  hs
  |> CursorPath_common.hooks_opseq(
       ~hooks_operand,
       ~hook,
       ~is_space,
       ~rev_steps,
       uty,
     )
and hooks_operand =
    (
      operand: UHTyp.operand,
      rev_steps: CursorPath.rev_steps,
      hs: CursorPath.hook_list,
    )
    : CursorPath.hook_list =>
  switch (operand) {
  | Hole => [mk_hook(TypHole, List.rev(rev_steps)), ...hs]
  | Unit
  | Int
  | Float
  | Bool => hs
  | Parenthesized(body)
  | List(body) => hs |> hooks(body, [0, ...rev_steps])
  };

let rec hooks_z =
        (zty: ZTyp.t, rev_steps: CursorPath.rev_steps): CursorPath.zhook_list =>
  hooks_zopseq(zty, rev_steps)
and hooks_zopseq =
    (zopseq: ZTyp.zopseq, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  CursorPath_common.hooks_zopseq_(
    ~hooks_operand,
    ~hooks_zoperand,
    ~hook,
    ~is_space,
    ~rev_steps,
    ~erase_zopseq=ZTyp.erase_zopseq,
    zopseq,
  )
and hooks_zoperand =
    (zoperand: ZTyp.zoperand, rev_steps: CursorPath.rev_steps)
    : CursorPath.zhook_list =>
  switch (zoperand) {
  | CursorT(_, Hole) =>
    CursorPath_common.mk_zhooks(
      ~hook_selected=Some(mk_hook(TypHole, List.rev(rev_steps))),
      (),
    )
  | CursorT(_, Unit | Int | Float | Bool) => CursorPath_common.no_hooks
  | CursorT(OnDelim(k, _), Parenthesized(body) | List(body)) =>
    let hooks = hooks(body, [0, ...rev_steps], []);
    switch (k) {
    | 0 => CursorPath_common.mk_zhooks(~hooks_before=hooks, ())
    | 1 => CursorPath_common.mk_zhooks(~hooks_after=hooks, ())
    | _ => CursorPath_common.no_hooks
    };
  | CursorT(OnOp(_) | OnText(_), Parenthesized(_) | List(_)) =>
    /* invalid cursor position */
    CursorPath_common.no_hooks
  | ParenthesizedZ(zbody)
  | ListZ(zbody) => hooks_z(zbody, [0, ...rev_steps])
  };

let prev_hole_steps_z = (zt: ZTyp.t): option(CursorPath.steps) =>
  hooks_z(zt, [])
  |> CursorPath.filter_holes_z
  |> CursorPath_common.prev_hook_steps;

let next_hole_steps_z = (zt: ZTyp.t): option(CursorPath.steps) => {
  hooks_z(zt, [])
  |> CursorPath.filter_holes_z
  |> CursorPath_common.next_hook_steps;
};

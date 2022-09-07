open Zipper;
open Util;
open OptUtil.Syntax;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type movability =
  | CanEnter(int, int)
  | CanPass
  | CantEven;

module Make = (M: Editor.Meta.S) => {
  let caret_point = Zipper.caret_point(M.measured);

  let movability = (chunkiness: chunkiness, label, delim_idx): movability => {
    assert(delim_idx < List.length(label));
    switch (chunkiness, label, delim_idx) {
    | (ByChar, _, _)
    | (MonoByChar, [_], 0) =>
      let char_max = Token.length(List.nth(label, delim_idx)) - 2;
      char_max < 0 ? CanPass : CanEnter(delim_idx, char_max);
    | (ByToken, _, _)
    | (MonoByChar, _, _) => CanPass
    };
  };

  let neighbor_movability =
      (chunkiness: chunkiness, {relatives: {siblings, ancestors}, _}: t)
      : (movability, movability) => {
    let movability = movability(chunkiness);
    let (supernhbr_l, supernhbr_r) =
      switch (ancestors) {
      | [] => (CantEven, CantEven)
      | [({children: (l_kids, _), label, _}, _), ..._] => (
          movability(label, List.length(l_kids)),
          movability(label, List.length(l_kids) + 1),
        )
      };
    let (l_nhbr, r_nhbr) = Siblings.neighbors(siblings);
    let l =
      switch (l_nhbr) {
      | Some(Tile({label, _})) => movability(label, List.length(label) - 1)
      | Some(_) => CanPass
      | _ => supernhbr_l
      };
    let r =
      switch (r_nhbr) {
      | Some(Tile({label, _})) => movability(label, 0)
      | Some(_) => CanPass
      | _ => supernhbr_r
      };
    (l, r);
  };

  let pop_out = Zipper.set_caret(Outer);
  let pop_move = (d, z, id_gen) => {
    let z = z |> Zipper.set_caret(Outer);
    Zipper.move(d, z, id_gen);
  };
  let inner_incr = (delim, c, z) =>
    Zipper.set_caret(Inner(delim, c + 1), z);
  let inner_decr = z => Some(Zipper.update_caret(Zipper.Caret.decrement, z));
  let inner_start = (d_init, z) => Zipper.set_caret(Inner(d_init, 0), z);
  let inner_end = (d, d_init, c_max, z, id_gen) => {
    let z = z |> Zipper.set_caret(Inner(d_init, c_max));
    Zipper.move(d, z, id_gen);
  };

  let primary =
      (chunkiness: chunkiness, d: Direction.t, z: t, id_gen: IdGen.state)
      : option((t, IdGen.state)) => {
    switch (d, z.caret, neighbor_movability(chunkiness, z)) {
    /* this case maybe shouldn't be necessary but currently covers an edge
       (select an open parens to left of a multichar token and press left) */
    | _ when z.selection.content != [] => pop_move(d, z, id_gen)
    | (Left, Outer, (CanEnter(dlm, c_max), _)) =>
      inner_end(d, dlm, c_max, z, id_gen)
    | (Left, Outer, _) => Zipper.move(d, z, id_gen)
    | (Left, Inner(_), _) when chunkiness == ByToken =>
      Some((pop_out(z), id_gen))
    | (Left, Inner(_), _) =>
      Some((Zipper.update_caret(Zipper.Caret.decrement, z), id_gen))
    | (Right, Outer, (_, CanEnter(d_init, _))) =>
      Some((inner_start(d_init, z), id_gen))
    | (Right, Outer, _) => Zipper.move(d, z, id_gen)
    | (Right, Inner(_, c), (_, CanEnter(_, c_max))) when c == c_max =>
      pop_move(d, z, id_gen)
    | (Right, Inner(_), _) when chunkiness == ByToken =>
      pop_move(d, z, id_gen)
    | (Right, Inner(delim, c), _) =>
      Some((inner_incr(delim, c, z), id_gen))
    };
  };

  let do_towards =
      (
        ~anchor: option(Measured.Point.t)=?,
        f: (Direction.t, t, IdGen.state) => option(state),
        goal: Measured.Point.t,
        z: Zipper.t,
        id_gen: IdGen.state,
      )
      : option(state) => {
    let init = caret_point(z);
    let d =
      goal.row < init.row || goal.row == init.row && goal.col < init.col
        ? Direction.Left : Right;

    let rec go = (prev: t, curr: t, id_gen) => {
      let curr_p = caret_point(curr);
      switch (
        Measured.Point.dcomp(d, curr_p.col, goal.col),
        Measured.Point.dcomp(d, curr_p.row, goal.row),
      ) {
      | (Exact, Exact) => (curr, id_gen)
      | (_, Over) => (prev, id_gen)
      | (_, Under)
      | (Under, Exact) =>
        switch (f(d, curr, id_gen)) {
        | None => (curr, id_gen)
        | Some((next, id_gen)) => go(curr, next, id_gen)
        }
      | (Over, Exact) =>
        switch (anchor) {
        | None =>
          let d_curr = abs(curr_p.col - goal.col);
          let d_prev = abs(caret_point(prev).col - goal.col);
          // default to going over when equal
          (d_prev < d_curr ? prev : curr, id_gen);
        | Some(anchor) =>
          let anchor_d =
            goal.row < anchor.row
            || goal.row == anchor.row
            && goal.col < anchor.col
              ? Direction.Left : Right;
          (anchor_d == d ? curr : prev, id_gen);
        }
      };
    };

    let (res, id_gen) = go(z, z, id_gen);
    Measured.Point.equals(caret_point(res), caret_point(z))
      ? None : Some((res, id_gen));
  };

  let do_vertical =
      (
        f: (Direction.t, t, IdGen.state) => option(state),
        d: Direction.t,
        z: t,
        id_gen,
      )
      : option(state) => {
    /* Here f should be a function which results in strict d-wards
       movement of the caret. Iterate f until we get to the closet
       caret position to a target derived from the initial position */
    let cur_p = caret_point(z);
    let goal =
      Measured.Point.{
        col: M.col_target,
        row: cur_p.row + (d == Right ? 1 : (-1)),
      };
    do_towards(f, goal, z, id_gen);
  };

  let do_extreme =
      (
        f: (Direction.t, t, IdGen.state) => option(state),
        d: planar,
        z: t,
        id_gen,
      )
      : option(state) => {
    let cur_p = caret_point(z);
    let goal: Measured.Point.t =
      switch (d) {
      | Right(_) => {col: Int.max_int, row: cur_p.row}
      | Left(_) => {col: 0, row: cur_p.row}
      | Up => {col: 0, row: 0}
      | Down => {col: Int.max_int, row: Int.max_int}
      };
    do_towards(f, goal, z, id_gen);
  };

  let vertical = (d: Direction.t, z: t, id_gen): option(state) =>
    z.selection.content == []
      ? do_vertical(primary(ByChar), d, z, id_gen)
      : Some((Zipper.directional_unselect(d, z), id_gen));

  let targets_within_row = (z: t, id_gen): list(state) => {
    let init = caret_point(z);
    let rec go = (d: Direction.t, z: t, id_gen) => {
      switch (primary(ByChar, d, z, id_gen)) {
      | None => []
      | Some((z, id_gen)) =>
        if (caret_point(z).row != init.row) {
          [];
        } else {
          switch (pop_backpack(z)) {
          | None => go(d, z, id_gen)
          | Some(_) => [(z, id_gen), ...go(d, z, id_gen)]
          };
        }
      };
    };
    let curr =
      switch (pop_backpack(z)) {
      | None => []
      | Some(_) => [(z, id_gen)]
      };
    List.rev(go(Left, z, id_gen)) @ curr @ go(Right, z, id_gen);
  };

  // TODO(d): unify this logic with rest of movement logic
  let rec to_backpack_target = (d: planar, z: t, id_gen): option(state) => {
    let done_or_try_again = (d, z, id_gen) =>
      switch (pop_backpack(z)) {
      | None => to_backpack_target(d, z, id_gen)
      | Some(_) => Some((z, id_gen))
      };
    switch (d) {
    | Left(chunk) =>
      let* (z, id_gen) = primary(chunk, Left, z, id_gen);
      done_or_try_again(d, z, id_gen);
    | Right(chunk) =>
      let* (z, id_gen) = primary(chunk, Right, z, id_gen);
      done_or_try_again(d, z, id_gen);
    | Up =>
      let* (z, id_gen) = vertical(Left, z, id_gen);
      let zs =
        targets_within_row(z, id_gen)
        |> List.sort(((z1, _), (z2, _)) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor left
             c != 0 ? c : Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z, id_gen)
      | [z_id, ..._] => Some(z_id)
      };
    | Down =>
      let* (z, id_gen) = vertical(Right, z, id_gen);
      let zs =
        targets_within_row(z, id_gen)
        |> List.sort(((z1, _), (z2, _)) => {
             let dist1 = caret_point(z1).col - M.col_target;
             let dist2 = caret_point(z2).col - M.col_target;
             let c = Int.compare(abs(dist1), abs(dist2));
             // favor right
             c != 0 ? c : - Int.compare(dist1, dist2);
           });
      switch (zs) {
      | [] => to_backpack_target(d, z, id_gen)
      | [z_id, ..._] => Some(z_id)
      };
    };
  };

  let to_start = do_extreme(primary(ByToken), Up);

  let go = (d: Action.move, z: Zipper.t, id_gen): option(state) =>
    switch (d) {
    | Goal(goal) =>
      let z = Zipper.unselect(z);
      do_towards(primary(ByChar), goal, z, id_gen);
    | Extreme(d) => do_extreme(primary(ByToken), d, z, id_gen)
    | Local(d) =>
      let f =
        switch (d) {
        | Left(chunk) => primary(chunk, Left)
        | Right(chunk) => primary(chunk, Right)
        | Up => vertical(Left)
        | Down => vertical(Right)
        };
      f(z, id_gen);
    };
};

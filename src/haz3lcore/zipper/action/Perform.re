open Util;
open Zipper;

let buffer_clear = (z: t): t =>
  switch (z.selection.mode) {
  | Buffer(_) => {...z, selection: Selection.mk([])}
  | _ => z
  };

let set_buffer = (info_map: Statics.Map.t, z: t): t =>
  switch (TyDi.set_buffer(~info_map, z)) {
  | None => z
  | Some(z) => z
  };

let go_z =
    (
      ~settings as _: CoreSettings.t,
      statics: CachedStatics.t,
      a: Action.t,
      module M: Move.S,
      z: Zipper.t,
    )
    : Action.Result.t(Zipper.t) => {
  module Move = Move.Make(M);
  module Select = Select.Make(M);

  let paste = (z: Zipper.t, str: string): option(Zipper.t) => {
    open Util.OptUtil.Syntax;
    let* z = Printer.zipper_of_string(~zipper_init=z, str);
    /* HACK(andrew): Insert/Destruct below is a hack to deal
       with the fact that pasting something like "let a = b in"
       won't trigger the barfing of the "in"; to trigger this,
       we insert a space, and then we immediately delete it */
    let* z = Insert.go(" ", z);
    let+ z = Destruct.go(Left, z);
    remold_regrout(Left, z);
  };

  let buffer_accept = (z): option(Zipper.t) =>
    switch (z.selection.mode) {
    | Normal => None
    | Buffer(Unparsed) =>
      switch (TyDi.get_buffer(z)) {
      | None => None
      | Some(completion)
          when StringUtil.match(StringUtil.regexp(".*\\)::$"), completion) =>
        /* Slightly hacky. There's currently only one genre of completion
         * that creates more than one hole on intial expansion: when on eg
         * 1 :: a|, we suggest "abs( )::" via lookahead. In such a case we
         * want the caret to end up to the left of the first hole, whereas
         * pasting would leave it to the left of the second. Thus we move
         * left to the previous hole. */
        let z = {
          open OptUtil.Syntax;
          let* z = paste(z, completion);
          let* z = Move.go(Goal(Piece(Grout, Left)), z);
          Move.go(Local(Left(ByToken)), z);
        };
        z;
      | Some(completion) => paste(z, completion)
      }
    };

  let smart_select = (n, z): option(Zipper.t) => {
    switch (n) {
    | 2 => Select.indicated_token(z)
    | 3 =>
      open OptUtil.Syntax;
      /* For things where triple-clicking would otherwise have
       * no additional effect, select the parent term instead */
      let* (p, _, _) = Indicated.piece''(z);
      Piece.is_term(p)
        ? Select.parent_of_indicated(z, statics.info_map)
        : Select.nice_term(z);
    | _ => None
    };
  };

  switch (a) {
  | Paste(clipboard) =>
    switch (paste(z, clipboard)) {
    | None => Error(CantPaste)
    | Some(z) => Ok(z)
    }
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    switch (Destruct.go(Left, z)) {
    | None => Error(Cant_destruct)
    | Some(z) => Ok(z)
    }
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    switch (Printer.reparse(z)) {
    | None => Error(CantReparse)
    | Some(z) => Ok(z)
    }
  | Buffer(Set(TyDi)) => Ok(set_buffer(statics.info_map, z))
  | Buffer(Accept) =>
    switch (buffer_accept(z)) {
    | None => Error(CantAccept)
    | Some(z) => Ok(z)
    }
  | Buffer(Clear) => Ok(buffer_clear(z))
  | Project(a) =>
    ProjectorPerform.go(
      Move.jump_to_id_indicated,
      Move.jump_to_side_of_id,
      a,
      z,
    )
  | Move(d) =>
    Move.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    (
      switch (jump_target) {
      | BindingSiteOfIndicatedVar =>
        open OptUtil.Syntax;
        let* idx = Indicated.index(z);
        let* ci = Id.Map.find_opt(idx, statics.info_map);
        let* binding_id = Info.get_binding_site(ci);
        Move.jump_to_id(z, binding_id);
      | TileId(id) => Move.jump_to_id(z, id)
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) =>
    let z = Zipper.directional_unselect(z.selection.focus, z);
    Ok(z);
  | Select(All) =>
    switch (Move.do_extreme(Move.primary(ByToken), Up, z)) {
    | Some(z) =>
      switch (Select.go(Extreme(Down), z)) {
      | Some(z) => Ok(z)
      | None => Error(Action.Failure.Cant_select)
      }
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Term(Current)) =>
    switch (Select.current_term(z)) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Smart(n)) =>
    switch (smart_select(n, z)) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Term(Id(id, d))) =>
    switch (Select.term(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Tile(Current)) =>
    switch (Select.current_tile(z)) {
    | None => Error(Cant_select)
    | Some(z) => Ok(z)
    }
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Resize(d)) =>
    Select.go(d, z) |> Result.of_option(~error=Action.Failure.Cant_select)
  | Destruct(d) =>
    z
    |> Destruct.go(d)
    |> Option.map(remold_regrout(d))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    z
    |> Insert.go(char)
    /* note: remolding here is done case-by-case */
    //|> Option.map((z) => remold_regrout(Right, z))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Zipper.pick_up(z)))
  | Put_down =>
    let z =
      /* Alternatively, putting down inside token could eiter merge-in or split */
      switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.put_down(Left, z)
      };
    z
    |> Option.map(remold_regrout(Left))
    |> Result.of_option(~error=Action.Failure.Cant_put_down);
  | RotateBackpack =>
    let z = {...z, backpack: Util.ListUtil.rotate(z.backpack)};
    Ok(z);
  | MoveToBackpackTarget((Left(_) | Right(_)) as d) =>
    if (Backpack.restricted(z.backpack)) {
      Move.to_backpack_target(d, z)
      |> Result.of_option(~error=Action.Failure.Cant_move);
    } else {
      Move.go(Local(d), z)
      |> Result.of_option(~error=Action.Failure.Cant_move);
    }
  | MoveToBackpackTarget((Up | Down) as d) =>
    Move.to_backpack_target(d, z)
    |> Result.of_option(~error=Action.Failure.Cant_move)
  };
};

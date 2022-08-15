open Util;
open Sexplib.Std;
open Zipper;

[@deriving (show({with_path: false}), sexp, yojson)]
type move =
  | Extreme(planar)
  | Local(planar)
  | Goal(Measured.point);

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(move)
    | Select(move)
    | Unselect
    | Destruct(Direction.t)
    | Insert(string)
    | RotateBackpack
    | MoveToBackpackTarget(planar)
    | Pick_up
    | Put_down;

  module Failure = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | Cant_move
      | Cant_insert
      | Cant_destruct
      | Cant_select
      | Cant_put_down;
  };

  module Result = {
    include Result;
    type t('success) = Result.t('success, Failure.t);
  };
};

let update_target = Caret.update_target;

let put_down = (z: t): option(t) =>
  /* Alternatively, putting down inside token could eiter merge-in or split */
  switch (z.caret) {
  | Inner(_) => None
  | Outer => Outer.put_down(z)
  };

let move = (d, z, id_gen) =>
  switch (d) {
  | Goal(goal) =>
    let z = Selection.is_empty(z.selection) ? z : Outer.unselect(z);
    Caret.do_towards(Move.primary(ByChar), goal, z)
    |> Option.map(update_target)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  | Extreme(d) =>
    Caret.do_extreme(Move.primary(ByToken), d, z)
    |> Option.map(update_target)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Local(d) =>
    /* Note: Don't update target on vertical movement */
    z
    |> (
      z =>
        switch (d) {
        | Left(chunk) =>
          Move.primary(chunk, Left, z) |> Option.map(update_target)
        | Right(chunk) =>
          Move.primary(chunk, Right, z) |> Option.map(update_target)
        | Up => Move.vertical(Left, z)
        | Down => Move.vertical(Right, z)
        }
    )
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move)
  };

let select_primary = (d: Direction.t, z: t): option(t) =>
  if (z.caret == Outer) {
    Outer.select(d, z);
  } else if (d == Left) {
    z
    |> Caret.set(Outer)
    |> Outer.move(Right)
    |> OptUtil.and_then(Outer.select(d));
  } else {
    z |> Caret.set(Outer) |> Outer.select(d);
  };

let select_vertical = (d: Direction.t, z: t): option(t) =>
  Caret.do_vertical(select_primary, d, z);

let select = (d, z, id_gen) =>
  (
    switch (d) {
    | Goal(goal) =>
      let anchor =
        Caret.point(
          Measured.of_segment(unselect_and_zip(z)),
          {...z, selection: Selection.toggle_focus(z.selection)},
        );
      Caret.do_towards(~anchor, select_primary, goal, z)
      |> Option.map(update_target);
    | Extreme(d) =>
      Caret.do_extreme(select_primary, d, z) |> Option.map(update_target)
    | Local(d) =>
      /* Note: Don't update target on vertical selection */
      switch (d) {
      | Left(_) => select_primary(Left, z) |> Option.map(update_target)
      | Right(_) => select_primary(Right, z) |> Option.map(update_target)
      | Up => select_vertical(Left, z)
      | Down => select_vertical(Right, z)
      }
    }
  )
  |> Option.map(IdGen.id(id_gen))
  |> Result.of_option(~error=Action.Failure.Cant_select);

let go = (a: Action.t, (z, id_gen): state): Action.Result.t(state) => {
  IncompleteBidelim.clear();
  switch (a) {
  | Move(d) => move(d, z, id_gen)
  | Unselect =>
    Ok((Outer.directional_unselect(z.selection.focus, z), id_gen))
  | Select(d) => select(d, z, id_gen)
  | Destruct(d) =>
    (z, id_gen)
    |> Destruct.go(d)
    |> Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    (z, id_gen)
    |> Insert.go(char)
    /* note: remolding here is done case-by-case */
    //|> Option.map(((z, id_gen)) => remold_regrout(Right, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_insert)
  | Pick_up => Ok(remold_regrout(Left, Outer.pick_up(z), id_gen))
  | Put_down =>
    z
    |> put_down
    |> Option.map(z => remold_regrout(Left, z, id_gen))
    |> Option.map(((z, id_gen)) => (update_target(z), id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | RotateBackpack =>
    Ok(({...z, backpack: Util.ListUtil.rotate(z.backpack)}, id_gen))
  | MoveToBackpackTarget(d) =>
    let map = Measured.of_segment(unselect_and_zip(z));
    Move.to_backpack_target(d, map, z)
    |> Option.map(IdGen.id(id_gen))
    |> Result.of_option(~error=Action.Failure.Cant_move);
  };
};

let drop_it_like_its_hot =
    (~move_first=false, z: Zipper.t, id_gen): (Zipper.t, int) => {
  //let id_zz = ref(id_gen);
  /* We start by moving... if we're in the middle of a token,
     to the end of the token, or if not, we move over by one
     token so that the CI for the indicated token (provided
     the rightwards token is indicated) will be based on the
     put-down semantics */
  let z =
    z.caret == Outer
      ? switch (Move.primary(ByToken, Right, z)) {
        | Some(z) when move_first => z
        | _ => z
        }
      : (
        switch (Move.pop_move(Right, z)) {
        | Some(z) => z
        | None => z
        }
      );
  //id_zz := id_gen;
  let drop_or_move = (z: Zipper.t): option(Zipper.t) => {
    switch (Outer.put_down(z)) {
    | None => Move.primary(ByToken, Right, z)
    | Some(z) =>
      //let id_gen = id_zz^;
      let (z, _id_gen) = remold_regrout(Left, z, id_gen);
      //id_zz := id_gen;
      Some(z);
    };
  };
  //let id_gen = id_zz^;
  (Caret.fixpoint(drop_or_move, z), id_gen);
};

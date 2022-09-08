open Sexplib.Std;
open Util;

module Meta = {
  type t = {
    touched: Touched.t,
    measured: Measured.t,
    col_target: int,
  };

  let init = (z: Zipper.t) => {
    touched: Touched.empty,
    measured: Measured.of_segment(Zipper.unselect_and_zip(z)),
    col_target: 0,
  };

  module type S = {
    let touched: Touched.t;
    let measured: Measured.t;
    let col_target: int;
  };
  let module_of_t = (m: t): (module S) =>
    (module
     {
       let touched = m.touched;
       let measured = m.measured;
       let col_target = m.col_target;
     });

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let next =
      (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, meta: t): t => {
    let {touched, measured, col_target} = meta;
    let touched = Touched.update(Time.tick(), effects, touched);
    let measured =
      Measured.of_segment(
        ~touched,
        ~old=measured,
        Zipper.unselect_and_zip(z),
      );
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Local(Up | Down)) => col_target
      | _ => Zipper.caret_point(measured, z).col
      };
    {touched, measured, col_target};
  };
};

module State = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    zipper: Zipper.t,
    [@opaque]
    meta: Meta.t,
  };

  let init = zipper => {zipper, meta: Meta.init(zipper)};

  let next = (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, state) => {
    zipper: z,
    meta: Meta.next(~effects, a, z, state.meta),
  };
};

module History = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type affix = list((Action.t, State.t));
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (affix, affix);

  let empty = ([], []);

  let add = (a: Action.t, state: State.t, (pre, _): t): t => (
    [(a, state), ...pre],
    [],
  );
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  state: State.t,
  history: History.t,
};

let init = z => {state: State.init(z), history: History.empty};
let empty = id => init(Zipper.init(id));

let get_seg = (ed: t) => Zipper.unselect_and_zip(ed.state.zipper);

let update_z = (f: Zipper.t => Zipper.t, ed: t) => {
  ...ed,
  state: {
    ...ed.state,
    zipper: f(ed.state.zipper),
  },
};
let put_z = (z: Zipper.t) => update_z(_ => z);

let update_z_opt = (f: Zipper.t => option(Zipper.t), ed: t) => {
  open OptUtil.Syntax;
  let+ z = f(ed.state.zipper);
  put_z(z, ed);
};

let new_state =
    (~effects: list(Effect.t)=[], a: Action.t, z: Zipper.t, ed: t): t => {
  let state = State.next(~effects, a, z, ed.state);
  let history = History.add(a, ed.state, ed.history);
  {state, history};
};

let caret_point = (ed: t): Measured.Point.t => {
  let State.{zipper, meta} = ed.state;
  Zipper.caret_point(meta.measured, zipper);
};

let undo = (ed: t) =>
  switch (ed.history) {
  | ([], _) => None
  | ([(a, prev), ...before], after) =>
    Some({state: prev, history: (before, [(a, ed.state), ...after])})
  };
let redo = (ed: t) =>
  switch (ed.history) {
  | (_, []) => None
  | (before, [(a, next), ...after]) =>
    Some({state: next, history: ([(a, ed.state), ...before], after)})
  };

let can_undo = ed => Option.is_some(undo(ed));
let can_redo = ed => Option.is_some(redo(ed));

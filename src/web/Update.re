open Sexplib.Std;
open Util;
open Core;

[@deriving (show({with_path: false}), sexp, yojson)]
type settings_action =
  | Captions
  | WhitespaceIcons
  | Statics
  | Dynamics
  | Student
  | ContextInspector
  | Mode(Model.mode);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Set(settings_action)
  | UpdateDoubleTap(option(float))
  | Mousedown
  | Mouseup
  | LoadInit
  | LoadDefault
  | Save
  | ToggleMode
  | SwitchEditor(int)
  | SetFontMetrics(FontMetrics.t)
  | SetLogoFontMetrics(FontMetrics.t)
  | PerformAction(Action.t)
  | FailedInput(FailedInput.reason) //TODO(andrew): refactor as failure?
  | Copy
  | Paste
  | Undo
  | Redo
  | SetShowBackpackTargets(bool)
  | MoveToNextHole(Direction.t);

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CantUndo
    | CantRedo
    | CantPaste
    | FailedToLoad
    | FailedToSwitch
    | UnrecognizedInput(FailedInput.reason)
    | FailedToPerform(Action.Failure.t)
    | Exception(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let save = (model: Model.t): unit =>
  switch (model.editors) {
  | Simple(ed) => LocalStorage.save_simple((model.id_gen, ed))
  | Study(n, eds) => LocalStorage.save_study((model.id_gen, n, eds))
  | School(n, eds) =>
    assert(n < List.length(eds));
    LocalStorage.save_school((model.id_gen, n, eds));
  };

let update_settings =
    (a: settings_action, settings: Model.settings): Model.settings => {
  let settings =
    switch (a) {
    | Statics =>
      /* NOTE: dynamics depends on statics, so if dynamics is on and
         we're turning statics off, turn dynamics off as well */
      {
        ...settings,
        statics: !settings.statics,
        dynamics: !settings.statics && settings.dynamics,
      }
    | Dynamics => {...settings, dynamics: !settings.dynamics}
    | Captions => {...settings, captions: !settings.captions}
    | WhitespaceIcons => {
        ...settings,
        whitespace_icons: !settings.whitespace_icons,
      }
    | Student => {...settings, student: !settings.student}
    | ContextInspector => {
        ...settings,
        context_inspector: !settings.context_inspector,
      }
    | Mode(mode) => {...settings, mode}
    };
  LocalStorage.save_settings(settings);
  settings;
};

let load_editor = (model: Model.t): Model.t =>
  switch (model.settings.mode) {
  | Simple =>
    let (id_gen, editor) = LocalStorage.load_simple();
    {...model, id_gen, editors: Simple(editor)};
  | Study =>
    let (id_gen, idx, editors) = LocalStorage.load_study();
    {...model, id_gen, editors: Study(idx, editors)};
  | School =>
    let (id_gen, idx, editors) = LocalStorage.load_school();
    {...model, id_gen, editors: School(idx, editors)};
  };

let load_default_editor = (model: Model.t): Model.t =>
  switch (model.settings.mode) {
  | Simple =>
    let (id_gen, editor) = Model.simple_init;
    {...model, editors: Simple(editor), id_gen};
  | Study =>
    let (id_gen, idx, editors) = Study.init;
    {...model, editors: Study(idx, editors), id_gen};
  | School =>
    let (id_gen, idx, editors) = School.init;
    {...model, editors: School(idx, editors), id_gen};
  };

let rotate_mode = (mode: Model.mode): Model.mode =>
  switch (mode) {
  | Simple => Study
  | Study => School
  | School => Simple
  };

let apply =
    (model: Model.t, update: t, _: State.t, ~schedule_action as _)
    : Result.t(Model.t) => {
  switch (update) {
  | Set(s_action) =>
    Ok({...model, settings: update_settings(s_action, model.settings)})
  | UpdateDoubleTap(double_tap) => Ok({...model, double_tap})
  | Mousedown => Ok({...model, mousedown: true})
  | Mouseup => Ok({...model, mousedown: false})
  | LoadInit =>
    // NOTE: load settings first to get last editor mode
    let model = {...model, settings: LocalStorage.load_settings()};
    Ok(load_editor(model));
  | LoadDefault => Ok(load_default_editor(model))
  | Save =>
    save(model);
    Ok(model);
  | SwitchEditor(n) =>
    switch (model.editors) {
    | Simple(_) => Error(FailedToSwitch)
    | Study(m, _) when m == n => Error(FailedToSwitch)
    | Study(_, zs) =>
      switch (n < List.length(zs)) {
      | false => Error(FailedToSwitch)
      | true =>
        LocalStorage.save_study((model.id_gen, n, zs));
        Ok({...model, editors: Study(n, zs)});
      }
    | School(m, _) when m == n => Error(FailedToSwitch)
    | School(_, zs) =>
      switch (n < List.length(zs)) {
      | false => Error(FailedToSwitch)
      | true =>
        LocalStorage.save_school((model.id_gen, n, zs));
        Ok({...model, editors: School(n, zs)});
      }
    }
  | ToggleMode =>
    let model = {
      ...model,
      settings:
        update_settings(
          Mode(rotate_mode(model.settings.mode)),
          model.settings,
        ),
    };
    Ok(load_editor(model));
  | SetShowBackpackTargets(b) => Ok({...model, show_backpack_targets: b})
  | SetFontMetrics(font_metrics) => Ok({...model, font_metrics})
  | SetLogoFontMetrics(logo_font_metrics) =>
    Ok({...model, logo_font_metrics})
  | PerformAction(a) =>
    let ed_init = Model.get_editor(model);
    switch (Core.Perform.go(a, ed_init, model.id_gen)) {
    | Error(err) => Error(FailedToPerform(err))
    | Ok((ed, id_gen)) =>
      Ok({...model, id_gen, editors: Model.put_editor(model, ed)})
    };
  | FailedInput(reason) => Error(UnrecognizedInput(reason))
  | Copy =>
    let clipboard = Printer.to_string_selection(Model.get_zipper(model));
    //JsUtil.copy_to_clipboard(clipboard);
    Ok({...model, clipboard});
  | Paste =>
    //let clipboard = JsUtil.get_from_clipboard();
    let clipboard = model.clipboard;
    let ed = Model.get_editor(model);
    switch (
      Printer.zipper_of_string(
        ~zipper_init=ed.state.zipper,
        model.id_gen,
        clipboard,
      )
    ) {
    | None => Error(CantPaste)
    | Some((z, id_gen)) =>
      //TODO: add correct action to history (Pick_up is wrong)
      let ed = Core.Editor.new_state(Pick_up, z, ed);
      Ok({...model, id_gen, editors: Model.put_editor(model, ed)});
    };
  | Undo =>
    let ed = Model.get_editor(model);
    switch (Core.Editor.undo(ed)) {
    | None => Error(CantUndo)
    | Some(ed) => Ok({...model, editors: Model.put_editor(model, ed)})
    };
  | Redo =>
    let ed = Model.get_editor(model);
    switch (Core.Editor.redo(ed)) {
    | None => Error(CantRedo)
    | Some(ed) => Ok({...model, editors: Model.put_editor(model, ed)})
    };
  | MoveToNextHole(_d) =>
    // TODO restore
    Ok(model)
  };
};

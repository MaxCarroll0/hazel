open Haz3lcore;

include UpdateAction; // to prevent circularity

let update_settings =
    (a: settings_action, {settings, _} as model: Model.t): Model.t =>
  switch (a) {
  | Statics =>
    /* NOTE: dynamics depends on statics, so if dynamics is on and
       we're turning statics off, turn dynamics off as well */
    {
      ...model,
      settings: {
        ...settings,
        core: {
          statics: !settings.core.statics,
          dynamics: !settings.core.statics && settings.core.dynamics,
        },
      },
    }
  | Dynamics => {
      ...model,
      settings: {
        ...settings,
        core: {
          ...settings.core,
          dynamics: !settings.core.dynamics,
        },
      },
    }
  | Benchmark => {
      ...model,
      settings: {
        ...settings,
        benchmark: !settings.benchmark,
      },
    }
  | Captions => {
      ...model,
      settings: {
        ...settings,
        captions: !settings.captions,
      },
    }
  | SecondaryIcons => {
      ...model,
      settings: {
        ...settings,
        secondary_icons: !settings.secondary_icons,
      },
    }
  | ContextInspector => {
      ...model,
      settings: {
        ...settings,
        context_inspector: !settings.context_inspector,
      },
    }
  | InstructorMode =>
    let new_mode = !settings.instructor_mode;
    {
      ...model,
      editors: Editors.set_instructor_mode(model.editors, new_mode),
      settings: {
        ...settings,
        instructor_mode: !settings.instructor_mode,
      },
    };
  | Mode(mode) => {
      ...model,
      settings: {
        ...settings,
        mode,
      },
    }
  };

let reevaluate_post_update =
  fun
  | Set(s_action) =>
    switch (s_action) {
    | Captions
    | SecondaryIcons
    | Statics
    | Benchmark => false
    | Dynamics
    | InstructorMode
    | ContextInspector
    | Mode(_) => true
    }
  | SetMeta(meta_action) =>
    switch (meta_action) {
    | DoubleTap(_)
    | Mousedown
    | Mouseup
    | ShowBackpackTargets(_)
    | FontMetrics(_)
    | Result(_) => false
    | MVU(_)
    | Auto(_) => true
    }
  | PerformAction(
      Move(_) | MoveToNextHole(_) | Select(_) | Unselect(_) | RotateBackpack |
      MoveToBackpackTarget(_) |
      Jump(_),
    )
  | MoveToNextHole(_)
  | Save
  | Copy
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateLangDocMessages(_)
  | DebugAction(_)
  | StoreKey(_) => false
  | ExportPersistentData => false
  // may not be necessary on all of these
  // TODO review and prune
  | ReparseCurrentEditor
  | PerformAction(Destruct(_) | Insert(_) | Pick_up | Put_down)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SwitchEditor(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | Cut
  | Paste(_)
  | Assistant(_)
  | Execute
  | Undo
  | Redo
  | Reset => true;

let evaluate_and_schedule =
    (_state: State.t, ~schedule_action as _, model: Model.t): Model.t => {
  let model = {
    ...model,
    meta: {
      ...model.meta,
      results:
        Util.TimeUtil.measure_time(
          "ModelResults.init", model.settings.benchmark, () =>
          ModelResults.init(
            model.settings.core.dynamics
              ? Editors.get_spliced_elabs(model.editors) : [],
          )
        ),
    },
  };

  // if (model.settings.dynamics) {
  //   Editors.get_spliced_elabs(model.editors)
  //   |> List.iter(((key, d)) => {
  //        /* Send evaluation request. */
  //        let pushed = State.evaluator_next(state, key, d);

  //        /* Set evaluation to pending after short timeout. */
  //        /* FIXME: This is problematic if evaluation finished in time, but UI hasn't
  //         * updated before below action is scheduled. */
  //        Delay.delay(
  //          () =>
  //            if (pushed |> Lwt.is_sleeping) {
  //              schedule_action(UpdateResult(key, ResultPending));
  //            },
  //          300,
  //        );
  //      });
  // };
  model;
};

let perform_action = (model: Model.t, a: Action.t): Result.t(Model.t) => {
  let (id, ed_init) = Editors.get_editor_and_id(model.editors);
  switch (Haz3lcore.Perform.go(a, ed_init, id)) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok((ed, id)) =>
    Ok({...model, editors: Editors.put_editor_and_id(id, ed, model.editors)})
  };
};

let switch_scratch_slide =
    (editors: Editors.t, ~instructor_mode, idx: int): option(Editors.t) =>
  switch (editors) {
  | DebugLoad
  | Examples(_) => None
  | Scratch(n, _) when n == idx => None
  | Scratch(_, slides) when idx >= List.length(slides) => None
  | Scratch(_, slides) => Some(Scratch(idx, slides))
  | Exercise(_, specs, _) when idx >= List.length(specs) => None
  | Exercise(_, specs, _) =>
    let spec = List.nth(specs, idx);
    let key = Exercise.key_of(spec);
    let exercise = Store.Exercise.load_exercise(key, spec, ~instructor_mode);
    Some(Exercise(idx, specs, exercise));
  };

let switch_exercise_editor =
    (editors: Editors.t, ~pos, ~instructor_mode): option(Editors.t) =>
  switch (editors) {
  | DebugLoad
  | Examples(_)
  | Scratch(_) => None
  | Exercise(m, specs, exercise) =>
    let exercise = Exercise.switch_editor(~pos, instructor_mode, ~exercise);
    Store.Exercise.save_exercise(exercise, ~instructor_mode);
    Some(Exercise(m, specs, exercise));
  };

/* This action saves a file which serializes all current editor
   settings, including the states of all Scratch and Example slides.
   This saved file can directly replace Haz3lweb/Init.ml, allowing
   you to make your current state the default startup state.

   This does NOT save any Exercises mode state or any langdocs
   state. The latter is intentional as we don't want to persist
   this between users. The former is a TODO, currently difficult
   due to the more complex architecture of Exercises. */
let export_persistent_data = () => {
  let data: PersistentData.t = {
    examples: Store.Examples.load() |> Store.Examples.to_persistent,
    scratch: Store.Scratch.load() |> Store.Scratch.to_persistent,
    settings: Store.Settings.load(),
  };
  let contents =
    "let startup : PersistentData.t = " ++ PersistentData.show(data);
  JsUtil.download_string_file(
    ~filename="Init.ml",
    ~content_type="text/plain",
    ~contents,
  );
  print_endline("INFO: Persistent data exported to Init.ml");
};

let rec apply =
        (model: Model.t, update: t, state: State.t, ~schedule_action)
        : Result.t(Model.t) => {
  let m: Result.t(Model.t) =
    switch (update) {
    | Reset => Ok(Model.reset(model))
    | Set(s_action) =>
      let model = update_settings(s_action, model);
      /* NOTE: Need to reload model for editors to load */
      Model.save(model);
      Ok(Model.load(model));
    | SetMeta(action) =>
      Ok({...model, meta: meta_update(model, action, ~schedule_action)})
    | UpdateLangDocMessages(u) =>
      let langDocMessages =
        LangDocMessages.set_update(model.langDocMessages, u);
      Model.save_and_return({...model, langDocMessages});
    | DebugAction(a) =>
      DebugAction.perform(a);
      Ok(model);
    | StoreKey(k, v) =>
      Store.Generic.save(k, v);
      Ok(model);
    | Execute =>
      let editor = model.editors |> Editors.get_editor;
      let str = Printer.to_string_selection(editor);
      print_endline("Execute: Parsing action: " ++ str);
      let update: UpdateAction.t =
        try(str |> Sexplib.Sexp.of_string |> t_of_sexp) {
        | _ =>
          print_endline("Execute: Action not recognized");
          Save;
        };
      apply(model, update, state, ~schedule_action);
    | Save => Model.save_and_return(model)
    | InitImportAll(file) =>
      JsUtil.read_file(file, data => schedule_action(FinishImportAll(data)));
      Ok(model);
    | FinishImportAll(data) =>
      switch (data) {
      | None => Ok(model)
      | Some(data) =>
        Export.import_all(data, ~specs=ExerciseSettings.exercises);
        Ok(Model.load(model));
      }
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      Ok(model);
    | FinishImportScratchpad(data) =>
      let editors = Editors.import_current(model.editors, data);
      Model.save_and_return({...model, editors});
    | ExportPersistentData =>
      export_persistent_data();
      Ok(model);
    | ResetCurrentEditor =>
      let instructor_mode = model.settings.instructor_mode;
      let editors = Editors.reset_current(model.editors, ~instructor_mode);
      Model.save_and_return({...model, editors});
    | SwitchScratchSlide(n) =>
      let instructor_mode = model.settings.instructor_mode;
      switch (switch_scratch_slide(model.editors, ~instructor_mode, n)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      };
    | SwitchExampleSlide(name) =>
      switch (Editors.switch_example_slide(model.editors, name)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      }
    | SwitchEditor(pos) =>
      let instructor_mode = model.settings.instructor_mode;
      switch (switch_exercise_editor(model.editors, ~pos, ~instructor_mode)) {
      | None => Error(FailedToSwitch)
      | Some(editors) => Model.save_and_return({...model, editors})
      };
    | PerformAction(Insert("?") as a) =>
      let editor = model.editors |> Editors.get_editor;
      let ctx_init = Editors.get_ctx_init(model.editors);
      UpdateAssistant.schedule_prompt(
        ~ctx_init,
        editor.state.zipper,
        ~schedule_action,
      );
      perform_action(model, a);
    | PerformAction(a) =>
      //NOTE: effectful
      switch (a) {
      | Destruct(_)
      | Insert(_) => schedule_action(Assistant(Prompt(TyDi)))
      | _ => ()
      };
      let model = UpdateAssistant.reset_buffer(model);
      perform_action(model, a);

    /*| Paste(clipboard) =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (
        Printer.zipper_of_string(~zipper_init=ed.state.zipper, id, clipboard)
      ) {
      | None => Error(CantPaste)
      | Some((z, id)) =>
        /* NOTE(andrew): These two perform calls are a hack to
           deal with the fact that pasting something like "let a = b in"
           won't trigger the barfing of the "in"; to trigger this, we
           insert a space, and then we immediately delete it. */
        switch (Haz3lcore.Perform.go_z(Insert(" "), z, id)) {
        | Error(_) => Error(CantPaste)
        | Ok((z, id)) =>
          switch (Haz3lcore.Perform.go_z(Destruct(Left), z, id)) {
          | Error(_) => Error(CantPaste)
          | Ok((z, id)) =>
            let ed = Haz3lcore.Editor.new_state(Pick_up, z, ed);
            //TODO: add correct action to history (Pick_up is wrong)
            let editors = Editors.put_editor_and_id(id, ed, model.editors);
            Ok({...model, editors});
          }
        }
      };*/
    | ReparseCurrentEditor =>
      /* This serializes the current editor to text, resets the current
         editor, and then deserializes. It is intended as a (tactical)
         nuclear option for weird backpack states */
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      let zipper_init = Zipper.init(id);
      let ed_str = Printer.to_string_editor(ed);
      switch (Printer.zipper_of_string(~zipper_init, id + 1, ed_str)) {
      | None => Error(CantReset)
      | Some((z, id)) =>
        //TODO: add correct action to history (Pick_up is wrong)
        let editor = Haz3lcore.Editor.new_state(Pick_up, z, ed);
        let editors = Editors.put_editor_and_id(id, editor, model.editors);
        Ok({...model, editors});
      };
    | Cut =>
      // system clipboard handling itself is done in Page.view handlers
      perform_action(model, Destruct(Left))
    | Copy =>
      // system clipboard handling itself is done in Page.view handlers
      // doesn't change the state but including as an action for logging purposes
      Ok(model)
    | Paste(clipboard) =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Printer.paste_into_zip(ed.state.zipper, id, clipboard)) {
      | None => Error(CantPaste)
      | Some((z, id)) =>
        //HACK(andrew): below is not strictly a insert action...
        let ed = Haz3lcore.Editor.new_state(Insert(clipboard), z, ed);
        let editors = Editors.put_editor_and_id(id, ed, model.editors);
        Ok({...model, editors});
      };

    | Undo =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Haz3lcore.Editor.undo(ed)) {
      | None => Error(CantUndo)
      | Some(ed) =>
        Ok({
          ...model,
          editors: Editors.put_editor_and_id(id, ed, model.editors),
        })
      };
    | Redo =>
      let (id, ed) = Editors.get_editor_and_id(model.editors);
      switch (Haz3lcore.Editor.redo(ed)) {
      | None => Error(CantRedo)
      | Some(ed) =>
        Ok({
          ...model,
          editors: Editors.put_editor_and_id(id, ed, model.editors),
        })
      };
    | MoveToNextHole(d) =>
      let p: Piece.t => bool = (
        fun
        | Grout(_) => true
        | _ => false
      );
      perform_action(model, Move(Goal(Piece(p, d))));
    | Assistant(action) =>
      UpdateAssistant.apply(
        model,
        action,
        ~schedule_action,
        ~state,
        ~main=apply,
      )
    };
  reevaluate_post_update(update)
    ? m |> Result.map(~f=evaluate_and_schedule(state, ~schedule_action)) : m;
}
and meta_update =
    (model: Model.t, update: set_meta, ~schedule_action): Model.meta => {
  switch (update) {
  | DoubleTap(double_tap) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        double_tap,
      },
    }
  | Mousedown => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        mousedown: true,
      },
    }
  | Mouseup => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        mousedown: false,
      },
    }
  | ShowBackpackTargets(b) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        show_backpack_targets: b,
      },
    }
  | FontMetrics(font_metrics) => {
      ...model.meta,
      ui_state: {
        ...model.meta.ui_state,
        font_metrics,
      },
    }
  | MVU(name, dh) => {
      ...model.meta,
      mvu_states: VarMap.extend(model.meta.mvu_states, (name, dh)),
    }
  | Result(key, res) =>
    /* If error, print a message. */
    switch (res) {
    | ResultFail(Program_EvalError(reason)) =>
      let serialized =
        reason |> EvaluatorError.sexp_of_t |> Sexplib.Sexp.to_string_hum;
      print_endline(
        "[Program.EvalError(EvaluatorError.Exception(" ++ serialized ++ "))]",
      );
    | ResultFail(Program_DoesNotElaborate) =>
      print_endline("[Program.DoesNotElaborate]")
    | _ => ()
    };
    let r =
      model.meta.results
      |> ModelResults.find(key)
      |> ModelResult.update_current(res);
    let results = model.meta.results |> ModelResults.add(key, r);
    {...model.meta, results};
  | Auto(action) => UpdateAuto.go(model, action, ~schedule_action)
  };
};

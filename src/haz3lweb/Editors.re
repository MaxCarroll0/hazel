open Sexplib.Std;
open Haz3lcore;
// open Util;
// open Init;
open ScratchSlide;

// module Init = Haz3lweb__Init;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = (string, list((string, ScratchSlide.state)));

[@deriving (show({with_path: false}), sexp, yojson)]
type exercises = (int, list(Exercise.spec), Exercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(int, list(ScratchSlide.state))
  | Documentation(string, list((string, ScratchSlide.state)))
  | Tutorial(string, list((string, DocumentationEnv.state)))
  | Exercises(int, list(Exercise.spec), Exercise.state);

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    List.nth(slides, n).hidden_tests.tests;
  | Documentation(name, slides) =>
    // assert(List.mem_assoc(name, slides));
    // let slide_state = List.assoc(name, slides);
    List.assoc(name, slides).hidden_tests.tests
  | Tutorial(name, slides) =>
    assert(List.mem_assoc(name, slides));
<<<<<<< Updated upstream
    let slide_state = List.assoc(name, slides);
    DocumentationEnv.editor_of_state(slide_state);
  // List.assoc(name, slides).eds.your_impl;
=======
    List.assoc(name, slides).hidden_tests.tests;
>>>>>>> Stashed changes
  | Exercises(_, _, exercise) => Exercise.editor_of_state(exercise)
  };

let update_assoc = ((k, v), lst) =>
  List.map(((k', v')) => k == k' ? (k, v) : (k', v'), lst);

let put_editor = (editor: Editor.t, eds: t): t =>
  switch (eds) {
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
<<<<<<< Updated upstream

    let new_ed: state = {
      title: "",
      description: "",
      hidden_tests: {
        tests: editor,
        hints: [],
      },
    };

    Scratch(n, Util.ListUtil.put_nth(n, new_ed, slides));

  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    let new_ed = {
      title: "",
      description: "",
      hidden_tests: {
        tests: editor,
        hints: [],
      },
    };
    Documentation(name, slides |> update_assoc((name, new_ed)));

  | Tutorial(name, slides) =>
    assert(List.mem_assoc(name, slides));

    // Function to update the slide based on `editor`
    let update_slide =
        (hint: string, state: DocumentationEnv.state)
        : (string, DocumentationEnv.state) =>
      if (hint == name) {
        // print_endline("hint == name");
        print_endline(hint);
        print_endline(name);

        let updatedState = DocumentationEnv.put_editor(state, editor);

        (hint, updatedState);
      } else {
        (
          // print_endline("different_slide");
          hint,
          state,
        );
      };

    // Map the update function over the slides
    let updatedSlides =
      List.map(
        slide => {
          let (hint, state) = slide;
          update_slide(hint, state);
        },
        slides,
      );

    Tutorial(name, updatedSlides);
=======
    let originalSlide = List.nth(slides, n);
    let updatedSlide: ScratchSlide.state = {
      title: originalSlide.title,
      description: originalSlide.description,
      hidden_tests: {
        tests: ed,
        hints: originalSlide.hidden_tests.hints,
      },
    };
    Scratch(n, Util.ListUtil.put_nth(n, updatedSlide, slides));
  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    let originalSlide = List.assoc(name, slides);
    let updatedSlide: ScratchSlide.state = {
      title: originalSlide.title,
      description: originalSlide.description,
      hidden_tests: {
        tests: ed,
        hints: originalSlide.hidden_tests.hints,
      },
    };
    Documentation(
      name,
      slides |> ListUtil.update_assoc((name, updatedSlide)),
    );
>>>>>>> Stashed changes
  | Exercises(n, specs, exercise) =>
    // For Exercises, update the hidden_tests with `editor`
    Exercises(n, specs, Exercise.put_editor(exercise, editor))
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let toEditor = (state: state): Editor.t => {
  switch (state) {
  | s => s.hidden_tests.tests
  };
};
let fromEditor = (editor: Editor.t): state => {
  title: "",
  description: "",
  hidden_tests: {
    tests: editor,
    hints: [],
  },
};

let get_ctx_init = (~settings as _: Settings.t, editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.ctx_init
  | Tutorial(_) => Builtins.ctx_init
  };

let get_env_init = (~settings as _: Settings.t, editors: t): Environment.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.env_init
  | Tutorial(_) => Builtins.env_init
  };

let mk_statics = (~settings: Settings.t, editors: t): CachedStatics.t => {
  let editor = get_editor(editors);
  let ctx_init = get_ctx_init(~settings, editors);
  switch (editors) {
  | _ when !settings.core.statics => CachedStatics.mk([])
  | Scratch(idx, _) =>
    let key = ScratchSlide.scratch_key(string_of_int(idx));
    [(key, ScratchSlide.mk_statics(~settings, editor, ctx_init))]
    |> CachedStatics.mk;
  | Documentation(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    [(key, ScratchSlide.mk_statics(~settings, editor, ctx_init))]
    |> CachedStatics.mk;
  | Tutorial(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    [(key, ScratchSlide.mk_statics(~settings, editor, ctx_init))]
    |> CachedStatics.mk;
  | Exercises(_, _, exercise) =>
    Exercise.mk_statics(settings.core, exercise) |> CachedStatics.mk
  };
};

let lookup_statics =
    (~settings: Settings.t, ~statics, editors: t): CachedStatics.statics =>
  switch (editors) {
  | _ when !settings.core.statics => CachedStatics.empty_statics
  | Scratch(idx, _) =>
    let key = ScratchSlide.scratch_key(string_of_int(idx));
    CachedStatics.lookup(statics, key);
  | Documentation(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    CachedStatics.lookup(statics, key);
  | Tutorial(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    CachedStatics.lookup(statics, key);
  | Exercises(_, _, exercise) =>
    let key = Exercise.key_for_statics(exercise);
    CachedStatics.lookup(statics, key);
  };

/* Each mode (e.g. Scratch, School) requires
   elaborating on some number of expressions
   that are spliced together from the editors
   in the mode. Each elaborated expression
   is given a key for later lookup by the mode.

   Used in the Update module */
let get_spliced_elabs =
    (~settings: Settings.t, statics, editors: t)
    : list((ModelResults.key, DHExp.t)) =>
  switch (editors) {
  | Scratch(idx, _) =>
    let key = ScratchSlide.scratch_key(idx |> string_of_int);
    let CachedStatics.{term, info_map, _} =
      lookup_statics(~settings, ~statics, editors);
    let d = Interface.elaborate(~settings=settings.core, info_map, term);
    [(key, d)];
  | Documentation(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    let CachedStatics.{term, info_map, _} =
      lookup_statics(~settings, ~statics, editors);
    let d = Interface.elaborate(~settings=settings.core, info_map, term);
    [(key, d)];
  | Tutorial(name, slides) =>
    let slideState = List.assoc(name, slides);
    DocumentationEnv.spliced_elabs(settings.core, slideState);
  | Exercises(_, _, exercise) =>
    Exercise.spliced_elabs(settings.core, exercise)
  };

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(n, slides) => Scratch(n, slides)
  | Documentation(_) => editors
  | Tutorial(name, slides) =>
    // Assuming you want to pass instructor_mode down to each slide
    let updated_slides =
      List.map(
        ((slide_name, slide_state)) => {
          (
            slide_name,
            DocumentationEnv.set_instructor_mode(
              slide_state,
              instructor_mode,
            ),
          )
        },
        slides,
      );
    Tutorial(name, updated_slides);
  | Exercises(n, specs, exercise) =>
    Exercises(
      n,
      specs,
      Exercise.set_instructor_mode(exercise, instructor_mode),
    )
  };

<<<<<<< Updated upstream
let reset_nth_slide = (n, slides) => {
  let (_, init_editors, _) = Init.startup.scratch;
  let data = List.nth(init_editors, n);
  let init_nth = ScratchSlide.unpersist(data);
  Util.ListUtil.put_nth(n, init_nth, slides);
};

// let reset_nth_slide_doc = (n, slides) => {
//   let (_, init_editors, _) = Init.startup.scratch;
//   let data = List.nth(init_editors, n);
//   let init_nth = DocumentationEnv.unpersist_state(data);
//   Util.ListUtil.put_nth(n, init_nth, slides);
// };

let reset_named_slide_2 = (name, slides) => {
  let (_, init_editors, _) = Init.startup.tutorial;
  let data = List.assoc(name, init_editors);
  let init_name = DocumentationEnv.unpersist_state(data);
  slides |> List.remove_assoc(name) |> List.cons((name, init_name));
=======
let reset_nth_slide =
    (~settings: CoreSettings.t, n, slides): list(ScratchSlide.state) => {
  let (_, init_editors, _) = Init.startup.scratch;
  let data = List.nth(init_editors, n);
  let init_nth = ScratchSlide.unpersist(~settings, data);
  let updated_slide: ScratchSlide.state = {
    title: init_nth.title,
    description: init_nth.description,
    hidden_tests: {
      tests: init_nth.hidden_tests.tests,
      hints: init_nth.hidden_tests.hints,
    },
  };
  Util.ListUtil.put_nth(n, updated_slide, slides);
};

let reset_named_slide =
    (~settings: CoreSettings.t, name, slides)
    : list((string, ScratchSlide.state)) => {
  let (_, init_editors, _) = Init.startup.documentation;
  let data = List.assoc(name, init_editors);
  let init_name = ScratchSlide.unpersist(~settings, data);
  let updated_slide: ScratchSlide.state = {
    title: init_name.title,
    description: init_name.description,
    hidden_tests: {
      tests: init_name.hidden_tests.tests,
      hints: init_name.hidden_tests.hints,
    },
  };
  slides |> List.remove_assoc(name) |> List.cons((name, updated_slide));
>>>>>>> Stashed changes
};

let reset_named_slide = (name, slides) => {
  let (_, init_editors, _) = Init.startup.documentation;
  let data = List.assoc(name, init_editors);
  let init_name = ScratchSlide.unpersist(data);
  slides |> List.remove_assoc(name) |> List.cons((name, init_name));
};

let reset_current = (editors: t, ~instructor_mode: bool): t =>
  switch (editors) {
  // trying to map to type state but func not working
  | Scratch(n, slides) =>
    let slides = List.map(toEditor, slides);
    let editorList = reset_nth_slide(n, slides);
    let editorList = List.map(fromEditor, editorList);
    Scratch(n, editorList);

  | Documentation(name, slides) =>
    let from_tup = ((word: string, status: state)) => (
      word,
      toEditor(status),
    );
    let slides = List.map(from_tup, slides);
    let slides = reset_named_slide(name, slides);
    let to_tup = ((word: string, editor: Editor.t)) => (
      word,
      fromEditor(editor),
    );
    let slides = List.map(to_tup, slides);
    Documentation(name, slides);

  | Tutorial(name, slides) =>
    let from_tup = ((word: string, status: DocumentationEnv.state)) => {
      // word,
      // toEditor(status),

      // let editor = toEditor(status); // Get the editor state
      let your_impl_zipper = status.eds.your_impl.state.zipper;
      let hidden_tests_zipper = status.eds.hidden_tests.tests.state.zipper;
      let spec: DocumentationEnv.spec = {
        title: status.eds.title,
        description: status.eds.description,
        your_impl: your_impl_zipper,
        hidden_tests: {
          tests: hidden_tests_zipper,
          hints: status.eds.hidden_tests.hints,
        },
      };

      (word, DocumentationEnv.state_of_spec(spec, ~instructor_mode));
    };

    let updatedSlides = List.map(from_tup, slides);

    Tutorial(name, updatedSlides);

  | Exercises(n, specs, _) =>
    Exercises(
      n,
      specs,
      List.nth(specs, n) |> Exercise.state_of_spec(~instructor_mode),
    )
  };

let import_current = (editors: t, data: option(string)): t =>
  switch (editors) {
  | Documentation(_)
  | Tutorial(_)
  | Exercises(_) => failwith("impossible")
  | Scratch(idx, slides) =>
    switch (data) {
    | None => editors
    | Some(data) =>
      // let state = ScratchSlide.import(data);
      // let updated_slides = Util.ListUtil.put_nth(idx, state, editorList);
      // let editorList = List.map(fromEditor, updated_slides);
      // Scratch(idx, editorList);
      let temp_editor = ScratchSlide.import(data);
      let state = {
        title: "",
        description: "",
        hidden_tests: {
          tests: temp_editor,
          hints: [],
        },
      };

      let updatedSlides: list(ScratchSlide.state) =
        Util.ListUtil.put_nth(idx, state, slides);
      Scratch(idx, updatedSlides);
    }
  };

let switch_example_slide = (editors: t, name: string): option(t) =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_) => None
  | Documentation(cur, slides)
      when !List.mem_assoc(name, slides) || cur == name =>
    None
  | Documentation(_, slides) => Some(Documentation(name, slides))
  | Tutorial(_, slides) => Some(Tutorial(name, slides))
  };

open Sexplib.Std;
open Util;
open EvaluatorMonad;
open EvaluatorMonad.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t)
  | Step(DHExp.t)
  | Pause(DHExp.t);

let unbox =
  fun
  | Step(d)
  | Pause(d)
  | BoxedValue(d)
  | Indet(d) => d;

let fast_equal = (r1, r2) =>
  switch (r1, r2) {
  | (Step(d1), Step(d2))
  | (Pause(d1), Pause(d2))
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equal(d1, d2)
  | _ => false
  };

let t_of_evaluator_result = (r: EvaluatorResult.t): t =>
  switch (r) {
  | BoxedValue(d) => BoxedValue(d)
  | Indet(d) => Indet(d)
  };

let evaluator_result_of_t = (r: t): EvaluatorResult.t =>
  switch (r) {
  | Step(d)
  | Pause(d)
  | BoxedValue(d) => BoxedValue(d)
  | Indet(d) => Indet(d)
  };

/**
  Alias for EvaluatorMonad.
 */
type m('a) = EvaluatorMonad.t('a);

let matches = Evaluator.matches;

let evaluate_extend_env = Evaluator.evaluate_extend_env;

let ground_cases_of = Evaluator.ground_cases_of;

let eval_bin_bool_op = Evaluator.eval_bin_bool_op;

let eval_bin_bool_op_short_circuit = Evaluator.eval_bin_bool_op_short_circuit;

let eval_bin_int_op = Evaluator.eval_bin_int_op;

let eval_bin_float_op = Evaluator.eval_bin_float_op;

let eval_bin_string_op = Evaluator.eval_bin_string_op;

let evaluate_ap_builtin = Evaluator.evaluate_ap_builtin;

[@deriving sexp]
type evaluator_option = {pause_subexpression: bool};

let default_option = {pause_subexpression: true};
let evaluate_all_option = {pause_subexpression: false};

let rec transition =
        (env: ClosureEnvironment.t, d: DHExp.t, opt: evaluator_option): m(t) => {
  /* print_endline( */
  /*   "transition: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)), */
  /* ); */
  // TODO: Investigate
  /* Increment number of evaluation steps (calls to `evaluate`). */
  // let* () = take_step;
  switch (d) {
  | BoundVar(x) =>
    let d =
      x
      |> ClosureEnvironment.lookup(env)
      |> OptUtil.get(() => {
           print_endline("FreeInvalidVar");
           raise(EvaluatorError.Exception(FreeInvalidVar(x)));
         });
    /* We need to call [evaluate] on [d] again since [env] does not store
     * final expressions. */
    Step(d) |> return;
  // transition(env, d, opt);

  | Sequence(d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(_d1)
    | Pause(_d1)
    | BoxedValue(_d1) => transition(env, d2, opt)
    /* FIXME THIS IS A HACK FOR 490; for now, just return evaluated d2 even
     * if evaluated d1 is indet. */
    | Indet(_d1) =>
      /* let* r2 = step(env, d2, opt); */
      /* switch (r2) { */
      /* | BoxedValue(d2) */
      /* | Indet(d2) => Indet(Sequence(d1, d2)) |> return */
      /* }; */
      transition(env, d2, opt)
    };

  | Let(dp, d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Pause(d1') => Pause(Let(dp, d1', d2)) |> return
    | Step(d1') => Step(Let(dp, d1', d2)) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      switch (matches(dp, d1')) {
      | IndetMatch
      | DoesNotMatch => Indet(Closure(env, Let(dp, d1, d2))) |> return
      | Matches(env') =>
        // let* env'' = evaluate_extend_env(env', env);
        // print_endline(
        //   "transition: Let: d2: "
        //   ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d2)),
        // );
        // transition(env, d2, opt);
        // Step(Closure(env'', Let(dp, d1', d2))) |> return;
        Step(Substitution.subst(env', d2)) |> return
      }
    };

  | FixF(f, _, d') =>
    let* env' = evaluate_extend_env(Environment.singleton((f, d)), env);
    transition(env', d', opt);

  | Fun(_) => BoxedValue(Closure(env, d)) |> return

  | Ap(d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(Ap(d1', d2)) |> return
    | Pause(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | Pause(d2')
      | BoxedValue(d2')
      | Indet(d2') => Pause(Ap(d1', d2')) |> return
      };
    | BoxedValue(TestLit(id)) => evaluate_test(env, id, d2, opt)
    | BoxedValue(Tag(_)) =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      // TODO: What does it mean?
      | Step(d2) => Step(Ap(d1, d2)) |> return
      | Pause(d2) => Pause(Ap(d1, d2)) |> return
      | BoxedValue(d2) => BoxedValue(Ap(d1, d2)) |> return
      | Indet(d2) => Indet(Ap(d1, d2)) |> return
      };
    | BoxedValue(Closure(closure_env, Fun(dp, _, d3)) as d1) =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | Pause(d2') => Pause(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        switch (matches(dp, d2)) {
        | DoesNotMatch
        | IndetMatch => Indet(Ap(d1, d2)) |> return
        // opt.pause_subexpression ? Pause(Ap(d1, d2')) : Indet(d)
        | Matches(env') =>
          // evaluate a closure: extend the closure environment with the
          // new bindings introduced by the function application.
          let* env = evaluate_extend_env(env', closure_env);
          if (opt.pause_subexpression) {
            let* r3 = quick_steps(env, d3, opt);
            switch (r3) {
            | Pause(_) => Pause(Ap(d1, d2')) |> return
            | _ => transition(env, d3, opt)
            };
          } else {
            transition(env, d3, opt);
          };
        }
      };
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | Pause(d2') => Pause(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        transition(
          env,
          Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'),
          opt,
        )
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFun");
      raise(EvaluatorError.Exception(InvalidBoxedFun(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(Ap(d1, d2')) |> return
      | Pause(d2') => Pause(Ap(d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2')) |> return
      };
    };

  | ApBuiltin(ident, args) =>
    let* r = evaluate_ap_builtin(env, ident, args);
    switch (r) {
    | BoxedValue(d) => Step(d) |> return
    | Indet(d) => Indet(d) |> return
    };

  | TestLit(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | Tag(_) => BoxedValue(d) |> return
  // Do nothing to tag and all kinds of literals.
  // What does this mean? -- Weijia
  // I don't know either, but I guess it for
  // ```hazel
  // test true end;
  // ```
  // You can see @ Evaluator.re, the none
  // step evaluator also doesn't nothing to it. -- Haoxiang

  | BinBoolOp(op, d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(BinBoolOp(op, d1', d2)) |> return
    | Pause(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
      | Pause(d2')
      | BoxedValue(d2')
      | Indet(d2') => Pause(BinBoolOp(op, d1', d2')) |> return
      };
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => Step(b3) |> return
      | None =>
        let* r2 = transition(env, d2, opt);
        switch (r2) {
        | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
        | Pause(d2') => Pause(BinBoolOp(op, d1, d2')) |> return
        | BoxedValue(BoolLit(b2)) =>
          Step(eval_bin_bool_op(op, b1, b2)) |> return
        | BoxedValue(d2') =>
          print_endline("InvalidBoxedBoolLit");
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')));
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
        };
      }
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedBoolLit");
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinBoolOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinBoolOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2')) |> return
      };
    };

  | BinIntOp(op, d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(BinIntOp(op, d1', d2)) |> return
    | Pause(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | Pause(d2')
      | BoxedValue(d2')
      | Indet(d2') => Pause(BinIntOp(op, d1', d2')) |> return
      };
    | BoxedValue(IntLit(n1) as d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinIntOp(op, d1, d2')) |> return
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Step(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
          |> return
        | _ => Step(eval_bin_int_op(op, n1, n2)) |> return
        }
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedIntLit1");
        print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d2')));
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')));
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedIntLit2");
      print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d1')));
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinIntOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinIntOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2')) |> return
      };
    };

  | BinFloatOp(op, d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(BinFloatOp(op, d1', d2)) |> return
    | Pause(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | Pause(d2')
      | BoxedValue(d2')
      | Indet(d2') => Pause(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(FloatLit(f1) as d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(FloatLit(f2)) =>
        Step(eval_bin_float_op(op, f1, f2)) |> return
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedFloatLit");
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')));
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedFloatLit");
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinFloatOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinFloatOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2')) |> return
      };
    };

  | BinStringOp(op, d1, d2) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(BinStringOp(op, d1', d2)) |> return
    | Pause(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | Pause(d2')
      | BoxedValue(d2')
      | Indet(d2') => Pause(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(StringLit(s1) as d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(StringLit(s2)) =>
        Step(eval_bin_string_op(op, s1, s2)) |> return
      | BoxedValue(d2') =>
        print_endline("InvalidBoxedStringLit");
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d2')));
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    | BoxedValue(d1') =>
      print_endline("InvalidBoxedStringLit");
      raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1')));
    | Indet(d1') =>
      let* r2 = transition(env, d2, opt);
      switch (r2) {
      | Step(d2') => Step(BinStringOp(op, d1, d2')) |> return
      | Pause(d2') => Pause(BinStringOp(op, d1, d2')) |> return
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinStringOp(op, d1', d2')) |> return
      };
    };

  | Inj(ty, side, d1) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(Inj(ty, side, d1')) |> return
    | Pause(d1') => Pause(Inj(ty, side, d1')) |> return
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1')) |> return
    | Indet(d1') => Indet(Inj(ty, side, d1')) |> return
    };

  | Tuple(ds) =>
    let+ drs =
      ds
      |> List.map(d => transition(env, d, opt) >>| (r => (d, r)))
      |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
          | (Pause(_), _) => (Pause(empty), [el, ...dst])
          | (_, Pause(el')) => (Pause(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.Tuple(ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Pause(_) => Pause(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | Prj(targ, n) =>
    // TODO:
    if (n < 0) {
      return(
        Indet(InvalidOperation(d, InvalidOperationError.InvalidProjection)),
      );
    } else {
      let* r = transition(env, targ, opt);
      switch (r) {
      | BoxedValue(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          return(BoxedValue(List.nth(ds, n)));
        }
      | Indet(Tuple(ds) as rv) =>
        if (n >= List.length(ds)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          return(Indet(List.nth(ds, n)));
        }
      | BoxedValue(Cast(targ', Prod(tys), Prod(tys')) as rv)
      | Indet(Cast(targ', Prod(tys), Prod(tys')) as rv) =>
        if (n >= List.length(tys)) {
          return(
            Indet(
              InvalidOperation(rv, InvalidOperationError.InvalidProjection),
            ),
          );
        } else {
          let ty = List.nth(tys, n);
          let ty' = List.nth(tys', n);
          transition(env, Cast(Prj(targ', n), ty, ty'), opt);
        }
      | _ => return(Indet(d))
      };
    }
  | Cons(d1, d2) =>
    let* r1 = transition(env, d1, opt);
    let* r2 = transition(env, d2, opt);
    switch (r1, r2) {
    | (Step(d1'), _) => Step(Cons(d1', d2)) |> return
    | (_, Step(d2')) => Step(Cons(d1, d2')) |> return
    | (Pause(d1'), _) => Pause(Cons(d1', d2)) |> return
    | (_, Pause(d2')) => Pause(Cons(d1, d2')) |> return
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2)) |> return
    | (BoxedValue(d1), BoxedValue(d2)) =>
      switch (d2) {
      | ListLit(x1, x2, x3, x4, lst) =>
        BoxedValue(ListLit(x1, x2, x3, x4, [d1, ...lst])) |> return
      | Cast(ListLit(x1, x2, x3, x4, lst), List(ty), List(ty')) =>
        BoxedValue(
          Cast(
            ListLit(x1, x2, x3, x4, [d1, ...lst]),
            List(ty),
            List(ty'),
          ),
        )
        |> return
      | _ =>
        print_endline("InvalidBoxedListLit");
        raise(EvaluatorError.Exception(InvalidBoxedListLit(d2)));
      }
    };

  | ListLit(u, i, err, ty, ds) =>
    let+ drs =
      ds
      |> List.map(d => transition(env, d, opt) >>| (r => (d, r)))
      |> sequence;

    let empty = DHExp.Tuple([]);
    let (tag, ds') =
      List.fold_right(
        ((el, r), (tag, dst)) => {
          switch (tag, r) {
          | (Step(_), _) => (Step(empty), [el, ...dst])
          | (_, Step(el')) => (Step(empty), [el', ...dst])
          | (Pause(_), _) => (Pause(empty), [el, ...dst])
          | (_, Pause(el')) => (Pause(empty), [el', ...dst])
          | (Indet(_), _) => (Indet(empty), [el, ...dst])
          | (_, Indet(el')) => (Indet(empty), [el', ...dst])
          | (BoxedValue(_), BoxedValue(el')) => (
              BoxedValue(empty),
              [el', ...dst],
            )
          }
        },
        drs,
        (BoxedValue(empty), []),
      );

    let d' = DHExp.ListLit(u, i, err, ty, ds');

    switch (tag) {
    | Step(_) => Step(d')
    | Pause(_) => Pause(d')
    | Indet(_) => Indet(d')
    | BoxedValue(_) => BoxedValue(d')
    };

  | ConsistentCase(Case(d1, rules, n)) =>
    evaluate_case(env, None, d1, rules, n, opt)

  /* Generalized closures evaluate to themselves. Only
     lambda closures are BoxedValues; other closures are all Indet. */
  | Closure(_, d') =>
    switch (d') {
    | Fun(_) => BoxedValue(d) |> return
    | _ => Indet(d) |> return
    }

  /* Hole expressions */
  | InconsistentBranches(u, i, Case(d1, rules, n)) =>
    evaluate_case(env, Some((u, i)), d1, rules, n, opt)

  | EmptyHole(u, i) => Indet(Closure(env, EmptyHole(u, i))) |> return

  | NonEmptyHole(reason, u, i, d1) =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') =>
      Step(Closure(env, NonEmptyHole(reason, u, i, d1'))) |> return
    | Pause(d1') =>
      Pause(Closure(env, NonEmptyHole(reason, u, i, d1'))) |> return
    | BoxedValue(d1')
    | Indet(d1') =>
      Indet(Closure(env, NonEmptyHole(reason, u, i, d1'))) |> return
    };

  | FreeVar(u, i, x) => Indet(Closure(env, FreeVar(u, i, x))) |> return

  | ExpandingKeyword(u, i, kw) =>
    Indet(Closure(env, ExpandingKeyword(u, i, kw))) |> return

  | InvalidText(u, i, text) =>
    Indet(Closure(env, InvalidText(u, i, text))) |> return

  /* Cast calculus */
  | Cast(d1, ty, ty') =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(Cast(d1', ty, ty')) |> return
    | Pause(d1') => Pause(Cast(d1', ty, ty')) |> return
    | BoxedValue(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1', ty, ty')) |> return
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            BoxedValue(d1'') |> return;
          } else {
            Indet(FailedCast(d1', ty, ty')) |> return;
          }
        | _ =>
          print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d1)));
          print_endline(Sexplib.Sexp.to_string_hum(Typ.sexp_of_t(ty)));
          print_endline(Sexplib.Sexp.to_string_hum(Typ.sexp_of_t(ty')));
          print_endline("CastBVHoleGround");
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')));
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d', opt);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d', opt);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1', ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          BoxedValue(Cast(d1', ty, ty')) |> return;
        }
      }
    | Indet(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result |> return
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result |> return
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1', ty, ty')) |> return
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Unknown(_)) =>
          if (Typ.eq(ty'', ty')) {
            Indet(d1'') |> return;
          } else {
            Indet(FailedCast(d1', ty, ty')) |> return;
          }
        | _ => Indet(Cast(d1', ty, ty')) |> return
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        transition(env, d', opt);
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        transition(env, d', opt);
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1', ty, ty')) |> return
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (Typ.eq(ty, ty')) {
          result |> return;
        } else {
          Indet(Cast(d1', ty, ty')) |> return;
        }
      }
    };

  | FailedCast(d1, ty, ty') =>
    let* r1 = transition(env, d1, opt);
    switch (r1) {
    | Step(d1') => Step(FailedCast(d1', ty, ty')) |> return
    | Pause(d1') => Step(FailedCast(d1', ty, ty')) |> return
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty')) |> return
    };

  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err)) |> return
  };
}

and evaluate_case =
    (
      env: ClosureEnvironment.t,
      inconsistent_info: option(HoleInstance.t),
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
      opt: evaluator_option,
    )
    : m(t) => {
  let* rscrut = transition(env, scrut, opt);
  switch (rscrut) {
  | Pause(scrut') =>
    let case = DHExp.Case(scrut', rules, current_rule_index);
    (
      switch (inconsistent_info) {
      | None => Pause(Closure(env, ConsistentCase(case)))
      | Some((u, i)) =>
        Pause(Closure(env, InconsistentBranches(u, i, case)))
      }
    )
    |> return;
  | Step(scrut') =>
    let case = DHExp.Case(scrut', rules, current_rule_index);
    (
      switch (inconsistent_info) {
      | None => Step(Closure(env, ConsistentCase(case)))
      | Some((u, i)) =>
        Step(Closure(env, InconsistentBranches(u, i, case)))
      }
    )
    |> return;
  | BoxedValue(scrut)
  | Indet(scrut) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      if (opt.pause_subexpression) {
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          switch (inconsistent_info) {
          | None => Pause(Closure(env, ConsistentCase(case)))
          | Some((u, i)) =>
            Pause(Closure(env, InconsistentBranches(u, i, case)))
          }
        )
        |> return;
      } else {
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          switch (inconsistent_info) {
          | None => Indet(Closure(env, ConsistentCase(case)))
          | Some((u, i)) =>
            Indet(Closure(env, InconsistentBranches(u, i, case)))
          }
        )
        |> return;
      }
    | Some(Rule(dp, d)) =>
      switch (matches(dp, scrut)) {
      | IndetMatch =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        (
          switch (inconsistent_info) {
          | None => Pause(ConsistentCase(case))
          | Some((u, i)) => Pause(InconsistentBranches(u, i, case))
          }
        )
        |> return;
      | Matches(env') =>
        // extend environment with new bindings introduced
        let* env = evaluate_extend_env(env', env);
        transition(env, d, opt);
      | DoesNotMatch =>
        evaluate_case(
          env,
          inconsistent_info,
          scrut,
          rules,
          current_rule_index + 1,
          opt,
        )
      }
    }
  };
}

and quick_steps =
    (env: ClosureEnvironment.t, d: DHExp.t, opt: evaluator_option): m(t) => {
  let* r = transition(env, d, opt);
  switch (r) {
  | Pause(d) => Pause(d) |> return
  | Step(d0) => quick_steps(env, d0, opt)
  | Indet(d) => Indet(d) |> return
  | BoxedValue(d) => BoxedValue(d) |> return
  };
}

and evaluate_test =
    (
      env: ClosureEnvironment.t,
      n: KeywordID.t,
      arg: DHExp.t,
      opt: evaluator_option,
    )
    : m(t) => {
  let* (arg_show, arg_result) =
    switch (DHExp.strip_casts(arg)) {
    | BinBoolOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinBoolOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, opt);
    | BinIntOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinIntOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, opt);
    | BinFloatOp(op, arg_d1, arg_d2) =>
      let mk_op = (arg_d1, arg_d2) => DHExp.BinFloatOp(op, arg_d1, arg_d2);
      evaluate_test_eq(env, mk_op, arg_d1, arg_d2, opt);

    | Ap(Ap(arg_d1, arg_d2), arg_d3) =>
      let* arg_d1 = transition(env, arg_d1, opt);
      let* arg_d2 = transition(env, arg_d2, opt);
      let* arg_d3 = transition(env, arg_d3, opt);
      let arg_show =
        DHExp.Ap(Ap(unbox(arg_d1), unbox(arg_d2)), unbox(arg_d3));
      let* arg_result = transition(env, arg_show, opt);
      (arg_show, arg_result) |> return;

    | Ap(arg_d1, arg_d2) =>
      let mk = (arg_d1, arg_d2) => DHExp.Ap(arg_d1, arg_d2);
      evaluate_test_eq(env, mk, arg_d1, arg_d2, opt);

    | _ =>
      let* arg = transition(env, arg, opt);
      (unbox(arg), arg) |> return;
    };

  let test_status: TestStatus.t =
    switch (arg_result) {
    | BoxedValue(BoolLit(true)) => Pass
    | BoxedValue(BoolLit(false)) => Fail
    | _ => Indet
    };

  let* _ = add_test(n, (arg_show, test_status));
  let r: t =
    switch (arg_result) {
    | BoxedValue(BoolLit(_)) => BoxedValue(Tuple([]))
    // TODO:
    | Step(arg)
    | Pause(arg)
    | BoxedValue(arg)
    | Indet(arg) => Indet(Ap(TestLit(n), arg))
    };
  r |> return;
}

and evaluate_test_eq =
    (
      env: ClosureEnvironment.t,
      mk_arg_op: (DHExp.t, DHExp.t) => DHExp.t,
      arg_d1: DHExp.t,
      arg_d2: DHExp.t,
      opt: evaluator_option,
    )
    : m((DHExp.t, t)) => {
  let* arg_d1 = transition(env, arg_d1, opt);
  let* arg_d2 = transition(env, arg_d2, opt);

  let arg_show = mk_arg_op(unbox(arg_d1), unbox(arg_d2));
  let* arg_result = transition(env, arg_show, opt);

  (arg_show, arg_result) |> return;
};

module EvalCtx = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Mark
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Pair1(t, DHExp.t)
    | Pair2(DHExp.t, t)
    | Let(DHPat.t, t, DHExp.t)
    | Inj(Typ.t, InjSide.t, t)
    | NonEmptyHole(ErrStatus.HoleReason.t, MetaVar.t, HoleInstanceId.t, t)
    | Cast(t, Typ.t, Typ.t)
    | FailedCast(t, Typ.t, Typ.t)
    | InvalidOperation(t, InvalidOperationError.t)
    | ConsistentCase(case)
    | InconsistentBranches(MetaVar.t, HoleInstanceId.t, case)
  and case =
    | Case(t, list(rule), int)
  and rule = DHExp.rule;
};

module EvalType = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Step
    | Pause;
};

// Fig.11 Final Forms
// d final
let is_final = (d: t): bool =>
  switch (d) {
  | Step(_) => false
  | Pause(_)
  | BoxedValue(_)
  | Indet(_) => true
  };

let is_pause = (d: t): bool => {
  switch (d) {
  | Step(_)
  | BoxedValue(_)
  | Indet(_) => false
  | Pause(_) => true
  };
};

module EvalObj = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    ctx: EvalCtx.t,
    exp: DHExp.t,
    typ: EvalType.t,
  };

  let mk = (ctx: EvalCtx.t, exp: DHExp.t): t => {
    ctx,
    exp,
    /* typ: is_pause(exp) ? Pause : Step, */
    typ: Pause,
  };
};

let rec decompose =
        (env: ClosureEnvironment.t, d: DHExp.t, opt: evaluator_option)
        : m(list(EvalObj.t)) => {
  let* r = transition(env, d, opt);
  // print_endline("decompose: " ++ Sexplib.Sexp.to_string_hum(sexp_of_t(r)));
  if (is_final(r)) {
    [] |> return;
  } else {
    switch (d) {
    | Closure(_)
    | Sequence(_)
    | ApBuiltin(_)
    | TestLit(_)
    | StringLit(_)
    | BinStringOp(_)
    | Tuple(_)
    | Prj(_)
    | Tag(_)
    | EmptyHole(_)
    | FreeVar(_)
    | InvalidText(_)
    | Fun(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | ListLit(_, _, _, _, _)
    | BoundVar(_)
    | ExpandingKeyword(_)
    | FixF(_, _, _) => [EvalObj.mk(Mark, d)] |> return
    | Ap(d1, d2) =>
      let* r1 = transition(env, d1, opt);
      let* r2 = transition(env, d2, opt);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        let* ld2 = decompose(env, d2, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(Ap1(obj.ctx, d2), obj.exp),
          ld1,
        )
        @ List.map(
            (obj: EvalObj.t): EvalObj.t =>
              EvalObj.mk(Ap2(d1, obj.ctx), obj.exp),
            ld2,
          )
        |> return;
      };
    | NonEmptyHole(reason, u, i, d1) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(NonEmptyHole(reason, u, i, obj.ctx), obj.exp),
          ld1,
        )
        |> return;
      };
    | BinBoolOp(op, d1, d2) =>
      let* r1 = transition(env, d1, opt);
      let* r2 = transition(env, d2, opt);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        let* ld2 = decompose(env, d2, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(BinBoolOp1(op, obj.ctx, d2), obj.exp),
          ld1,
        )
        @ List.map(
            (obj: EvalObj.t): EvalObj.t =>
              EvalObj.mk(BinBoolOp2(op, d1, obj.ctx), obj.exp),
            ld2,
          )
        |> return;
      };
    | BinIntOp(op, d1, d2) =>
      let* r1 = transition(env, d1, opt);
      let* r2 = transition(env, d2, opt);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        let* ld2 = decompose(env, d2, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(BinIntOp1(op, obj.ctx, d2), obj.exp),
          ld1,
        )
        @ List.map(
            (obj: EvalObj.t): EvalObj.t =>
              EvalObj.mk(BinIntOp2(op, d1, obj.ctx), obj.exp),
            ld2,
          )
        |> return;
        // BinIntOp1(op, (Mark, d1), d2)
        // BinIntOp2(op, d1, (Mark, d2))
      };
    | BinFloatOp(op, d1, d2) =>
      let* r1 = transition(env, d1, opt);
      let* r2 = transition(env, d2, opt);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        let* ld2 = decompose(env, d2, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(BinFloatOp1(op, obj.ctx, d2), obj.exp),
          ld1,
        )
        @ List.map(
            (obj: EvalObj.t): EvalObj.t =>
              EvalObj.mk(BinFloatOp2(op, d1, obj.ctx), obj.exp),
            ld2,
          )
        |> return;
      };
    | Cons(d1, d2) =>
      let* r1 = transition(env, d1, opt);
      let* r2 = transition(env, d2, opt);
      if (is_final(r1) && is_final(r2)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        let* ld2 = decompose(env, d2, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(Cons1(obj.ctx, d2), obj.exp),
          ld1,
        )
        @ List.map(
            (obj: EvalObj.t): EvalObj.t =>
              EvalObj.mk(Cons2(d1, obj.ctx), obj.exp),
            ld2,
          )
        |> return;
      };
    | Cast(d1, ty1, ty2) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(Cast(obj.ctx, ty1, ty2), obj.exp),
          ld1,
        )
        |> return;
      };
    | FailedCast(d1, ty1, ty2) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(FailedCast(obj.ctx, ty1, ty2), obj.exp),
          ld1,
        )
        |> return;
      };
    // | Pair(d1, d2) =>
    //   if (is_final(d1, opt) && is_final(d2, opt)) {
    //     [EvalObj.mk(Mark, d)];
    //   } else {
    //     let ld1 = decompose_all(d1, opt);
    //     let ld2 = decompose_all(d2, opt);
    //     List.map(
    //       (obj: EvalObj.t): EvalObj.t =>
    //         EvalObj.mk(Pair1(obj.ctx, d2), obj.exp),
    //       ld1,
    //     )
    //     @ List.map(
    //         (obj: EvalObj.t): EvalObj.t =>
    //           EvalObj.mk(Pair2(d1, obj.ctx), obj.exp),
    //         ld2,
    //       );
    //   }
    | Let(dp, d1, d2) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(Let(dp, obj.ctx, d2), obj.exp),
          ld1,
        )
        |> return;
      };
    | Inj(ty, side, d1) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(Inj(ty, side, obj.ctx), obj.exp),
          ld1,
        )
        |> return;
      };
    | InvalidOperation(d1, err) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(InvalidOperation(obj.ctx, err), obj.exp),
          ld1,
        )
        |> return;
      };
    | ConsistentCase(Case(d1, rule, n)) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(ConsistentCase(Case(obj.ctx, rule, n)), obj.exp),
          ld1,
        )
        |> return;
      };
    | InconsistentBranches(u, i, Case(d1, rule, n)) =>
      let* r1 = transition(env, d1, opt);
      if (is_final(r1)) {
        [EvalObj.mk(Mark, d)] |> return;
      } else {
        let* ld1 = decompose(env, d1, opt);
        List.map(
          (obj: EvalObj.t): EvalObj.t =>
            EvalObj.mk(
              InconsistentBranches(u, i, Case(obj.ctx, rule, n)),
              obj.exp,
            ),
          ld1,
        )
        |> return;
      };
    };
  };
};

let rec compose = ((ctx, d): (EvalCtx.t, DHExp.t)): DHExp.t => {
  print_endline(
    "compose: ctx: " ++ Sexplib.Sexp.to_string_hum(EvalCtx.sexp_of_t(ctx)),
  );
  print_endline(
    "compose: d: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  );
  switch (ctx) {
  | Mark => d
  | Ap1(ctx1, d1) => Ap(compose((ctx1, d)), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose((ctx1, d)))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose((ctx1, d)), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose((ctx1, d)))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose((ctx1, d)), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose((ctx1, d)))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose((ctx1, d)), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose((ctx1, d)))
  | Cons1(ctx1, d1) => Cons(compose((ctx1, d)), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose((ctx1, d)))
  // TODO: Pair -> Tuple
  | Pair1(ctx1, d1) => Tuple([compose((ctx1, d)), d1])
  | Pair2(d1, ctx1) => Tuple([d1, compose((ctx1, d))])
  | Let(dp, ctx1, d1) => Let(dp, compose((ctx1, d)), d1)
  | Inj(ty, side, ctx1) => Inj(ty, side, compose((ctx1, d)))
  | Cast(ctx1, ty1, ty2) => Cast(compose((ctx1, d)), ty1, ty2)
  | FailedCast(ctx1, ty1, ty2) => FailedCast(compose((ctx1, d)), ty1, ty2)
  | InvalidOperation(ctx1, err) => InvalidOperation(compose((ctx1, d)), err)
  | NonEmptyHole(reason, u, i, ctx1) =>
    NonEmptyHole(reason, u, i, compose((ctx1, d)))
  | ConsistentCase(Case(ctx1, rule, n)) =>
    ConsistentCase(Case(compose((ctx1, d)), rule, n))
  | InconsistentBranches(u, i, Case(ctx1, rule, n)) =>
    InconsistentBranches(u, i, Case(compose((ctx1, d)), rule, n))
  };
};

// let step =
//     (env: ClosureEnvironment.t, obj: EvalObj.t, opt: evaluator_option)
//     : m((t, list(EvalObj.t))) => {
//   let* (d, r) =
//     switch (obj.typ) {
//     | Pause => (compose((obj.ctx, obj.exp)), Pause(obj.exp)) |> return
//     | Step =>
//       let* r = transition(env, obj.exp, opt);
//       switch (r) {
//       | Pause(d)
//       | BoxedValue(d)
//       | Indet(d)
//       | Step(d) => (compose((obj.ctx, d)), r) |> return
//       };
//     };
//   print_endline(
//     "composed: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
//   );
//   let* ld = decompose(env, d, opt);
//   (r, ld) |> return;
// };

let step =
    (env: ClosureEnvironment.t, d: DHExp.t, ind: int, opt: evaluator_option)
    : m(t) =>
  if (ind == (-1)) {
    BoxedValue(d) |> return;
  } else {
    let* r = transition(env, d, opt);
    if (is_final(r)) {
      BoxedValue(d) |> return;
    } else {
      let* ld = decompose(env, d, opt);
      let obj = List.nth(ld, ind);
      let* r = transition(env, obj.exp, opt);
      (
        switch (r) {
        | Pause(d) => Pause(compose((obj.ctx, d)))
        | BoxedValue(d) => BoxedValue(compose((obj.ctx, d)))
        | Indet(d) => Indet(compose((obj.ctx, d)))
        | Step(d) => Step(compose((obj.ctx, d)))
        }
      )
      |> return;
    };
  };

/* let obj_step = (obj: EvalObj.t, opt: evaluator_option): DHExp.t => { */
/*   switch (step(obj.exp, opt)) { */
/*   | Pause(d0') */
/*   | BoxedValue(d0') */
/*   | Indet(d0') */
/*   | Step(d0') => compose((obj.ctx, d0')) */
/*   }; */
/* }; */

/* let rec ctx_steps = (d: DHExp.t, opt: evaluator_option): DHExp.t => { */
/*   let d' = ctx_step(d, opt); */
/*   if (is_final(d', opt)) { */
/*     d'; */
/*   } else { */
/*     ctx_steps(d', opt); */
/*   }; */
/* }; */

/* let step_evaluate = (d: DHExp.t, opt: evaluator_option): option(DHExp.t) => */
/*   try(Some(ctx_steps(d, opt))) { */
/*   | _ => None */
/*   }; */

let quick_step_evaluate =
    (env: ClosureEnvironment.t, d: DHExp.t, opt: evaluator_option)
    : m(EvaluatorResult.t) => {
  let* r = quick_steps(env, d, opt);
  switch (r) {
  | Pause(d)
  | Step(d)
  | Indet(d) => EvaluatorResult.Indet(d) |> return
  | BoxedValue(d) => EvaluatorResult.BoxedValue(d) |> return
  };
};

/* let rec record = */
/*         (env: ClosureEnvironment.t, d: DHExp.t, opt: evaluator_option) */
/*         : m(list(t)) => { */
/*   let* r' = step(env, d, opt); */
/*   if (is_final(r')) { */
/*     [r'] |> return; */
/*   } else { */
/*     let+ lst = record(env, unbox(r'), opt); */
/*     [r', ...lst]; */
/*   }; */
/* }; */
/**/
/* let record = (env: Environment.t, d: DHExp.t): (state, list(t)) => { */
/*   let es = EvaluatorState.init; */
/*   let (env, es) = */
/*     es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env)); */
/*   let opt = default_option; */
/*   record(env, d, opt, es); */
/* }; */

/* let ctx_step_index = (d: DHExp.t, opt: evaluator_option, index: int): DHExp.t => */
/*   if (is_final(d, opt)) { */
/*     d; */
/*   } else { */
/*     let ld = decompose_all(d, opt); */
/*     let ctx = List.nth(ld, index).ctx; */
/*     let d0 = List.nth(ld, index).exp; */
/*     switch (step(d0, opt)) { */
/*     | Pause(d0') */
/*     | BoxedValue(d0') */
/*     | Indet(d0') */
/*     | Step(d0') => compose((ctx, d0')) */
/*     }; */
/*   }; */

let step = (env: Environment.t, d: DHExp.t, ind: int, opt: evaluator_option) => {
  // print_endline(
  //   "stepping: " ++ Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d)),
  // );
  let es = EvaluatorState.init;
  let (env, es) =
    es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
  step(env, d, ind, opt, es);
};

// let step = (env: Environment.t, obj: EvalObj.t, opt: evaluator_option) => {
//   print_endline(
//     "stepping: " ++ Sexplib.Sexp.to_string_hum(EvalObj.sexp_of_t(obj)),
//   );
//   let es = EvaluatorState.init;
//   let (env, es) =
//     es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
//   step(env, obj, opt, es);
// };

let decompose = (env: Environment.t, d: DHExp.t, opt: evaluator_option) => {
  let es = EvaluatorState.init;
  let (env, es) =
    es |> EvaluatorState.with_eig(ClosureEnvironment.of_environment(env));
  decompose(env, d, opt, es);
};

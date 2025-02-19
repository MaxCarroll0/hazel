open Haz3lmenhir;
open Alcotest;
open Haz3lcore;

let alco_check =
  testable(
    Fmt.using(Haz3lcore.Exp.show, Fmt.string),
    Haz3lcore.DHExp.fast_equal,
  )
  |> Alcotest.check;

let strip_parens_and_add_builtins =
  Exp.map_term(
    ~f_exp=
      (cont: TermBase.exp_t => TermBase.exp_t, e: TermBase.exp_t) =>
        switch (e.term) {
        | Parens(e) => cont(e)
        | Var(x) =>
          let builtin =
            VarMap.lookup(Haz3lcore.Builtins.Pervasives.builtins, x);
          cont(
            switch (builtin) {
            | Some(Fn(_, _, _)) => cont(BuiltinFun(x) |> Exp.fresh)
            | Some(Const(_, _))
            | None => cont(e)
            },
          );
        | _ => cont(e)
        },
    ~f_pat=
      (cont, e) =>
        switch (e.term) {
        | Parens(e) => cont(e)
        | _ => cont(e)
        },
    ~f_typ=
      (cont, e) =>
        switch (e.term) {
        | Parens(e) => cont(e)
        | _ => cont(e)
        },
    _,
  );

// Existing recovering parser
let make_term_parse = (s: string) =>
  strip_parens_and_add_builtins(
    MakeTerm.from_zip_for_sem(Option.get(Printer.zipper_of_string(s))).term,
  );

let menhir_matches = (exp: Term.Exp.t, actual: string) =>
  alco_check(
    "menhir matches expected parse",
    exp,
    Haz3lmenhir.Conversion.Exp.of_menhir_ast(
      Haz3lmenhir.Interface.parse_program(actual),
    ),
  );

let menhir_only_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(name, `Quick, () => {menhir_matches(exp, actual)});

let skip_menhir_maketerm_equivalent_test =
    (~speed_level=`Quick, name: string, _actual: string) =>
  test_case(name, speed_level, () => {Alcotest.skip()});

let full_parser_test = (name: string, exp: Term.Exp.t, actual: string) =>
  test_case(
    name,
    `Quick,
    () => {
      alco_check(
        "expected parse matches MakeTerm parse",
        exp,
        make_term_parse(actual),
      );
      menhir_matches(exp, actual);
    },
  );

let menhir_maketerm_equivalent_test =
    (~speed_level=`Quick, name: string, actual: string) =>
  test_case(name, speed_level, () => {
    alco_check(
      "Menhir parse matches MakeTerm parse",
      make_term_parse(actual),
      Haz3lmenhir.Conversion.Exp.of_menhir_ast(
        Haz3lmenhir.Interface.parse_program(actual),
      ),
    )
  });

/**
 * QCheck Test to check the equivalence of the Menhir and MakeTerm parsing.
 * We generate an expression, convert it to the core representation, convert it to a segment,
 * serialize it, parse it with MakeTerm, and parse it with Menhir.
 */
let qcheck_menhir_maketerm_equivalent_test =
  QCheck.Test.make(
    ~name="Menhir and maketerm are equivalent",
    ~count=100,
    QCheck.make(~print=AST.show_exp, AST.gen_exp_sized(7)),
    exp => {
      let core_exp = Conversion.Exp.of_menhir_ast(exp);

      let segment =
        ExpToSegment.exp_to_segment(
          ~settings=
            ExpToSegment.Settings.of_core(~inline=true, CoreSettings.off),
          core_exp,
        );

      let serialized = Printer.of_segment(~holes=Some("?"), segment);
      let make_term_parsed = make_term_parse(serialized);
      let menhir_parsed = Haz3lmenhir.Interface.parse_program(serialized);
      let menhir_parsed_converted =
        Haz3lmenhir.Conversion.Exp.of_menhir_ast(menhir_parsed);

      switch (
        Haz3lcore.DHExp.fast_equal(make_term_parsed, menhir_parsed_converted)
      ) {
      | true => true
      | false => false
      | exception (Failure(msg)) =>
        print_endline("Error: " ++ msg);
        msg == "Sum type has non-unique constructors";
      };
    },
  );

/**
 * QCheck Test to check that menhir parses out what ExpToSegment serializes.
 * We generate an expression, convert it to the core representation, convert it to a segment,
 * serialize it, parse it with Menhir, and compare to the original.
 *
 * TODO This fails due to types not being serialized on constructors
 *  and some other ExpToSegment inconsistencies
 *
 * Filter and Test not implemented
 * Deferral serializing as "deferral"
 * Right associated operator
 * https://github.com/hazelgrove/hazel/issues/1452
 * https://github.com/hazelgrove/hazel/issues/1451
 * https://github.com/hazelgrove/hazel/issues/1445
 */
let qcheck_menhir_serialized_equivalent_test =
  QCheck.Test.make(
    ~name="Menhir through ExpToSegment and back",
    ~count=1000,
    QCheck.make(~print=AST.show_exp, AST.gen_exp_sized(7)),
    exp => {
      let core_exp = Conversion.Exp.of_menhir_ast(exp);

      let segment =
        ExpToSegment.exp_to_segment(
          ~settings={
            inline: true,
            fold_case_clauses: false,
            fold_fn_bodies: false,
            hide_fixpoints: false,
            fold_cast_types: false,
            show_filters: true,
          },
          core_exp,
        );
      let serialized = Printer.of_segment(~holes=Some("?"), segment);
      let menhir_parsed = Haz3lmenhir.Interface.parse_program(serialized);
      AST.equal_exp(menhir_parsed, exp);
    },
  );

let tests = (
  "MenhirParser",
  [
    full_parser_test("Integer Literal", Int(8) |> Exp.fresh, "8"),
    full_parser_test(
      "Fun",
      Fun(Var("x") |> Pat.fresh, Var("x") |> Exp.fresh, None, None)
      |> Exp.fresh,
      "fun x -> x",
    ),
    full_parser_test(
      "String Literal",
      String("Hello World") |> Exp.fresh,
      {|"Hello World"|},
    ),
    full_parser_test("Bool Literal", Bool(true) |> Exp.fresh, "true"),
    full_parser_test("Empty Hole", EmptyHole |> Exp.fresh, "?"),
    full_parser_test("Var", Var("x") |> Exp.fresh, "x"),
    full_parser_test(
      "Parens",
      Parens(Var("y") |> Exp.fresh) |> Exp.fresh,
      "(y)",
    ),
    full_parser_test(
      "BinOp",
      BinOp(Int(Plus), Int(4) |> Exp.fresh, Int(5) |> Exp.fresh)
      |> Exp.fresh,
      "4 + 5",
    ),
    full_parser_test(
      "Let",
      Let(
        Var("x") |> Pat.fresh,
        Int(5) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x = 5 in x",
    ),
    full_parser_test(
      "Tuple",
      Tuple([Int(4) |> Exp.fresh, Int(5) |> Exp.fresh]) |> Exp.fresh,
      "(4, 5)",
    ),
    full_parser_test(
      "Match",
      Match(
        Int(4) |> Exp.fresh,
        [
          (Int(1) |> Pat.fresh, String("hello") |> Exp.fresh),
          (Wild |> Pat.fresh, String("world") |> Exp.fresh),
        ],
      )
      |> Exp.fresh,
      {|case 4
         | 1 => "hello"
         | _ => "world"
        end|},
    ),
    full_parser_test(
      "If",
      If(Bool(true) |> Exp.fresh, Int(8) |> Exp.fresh, Int(6) |> Exp.fresh)
      |> Exp.fresh,
      "if true then 8 else 6",
    ),
    full_parser_test(
      "Deferred Ap",
      DeferredAp(Var("x") |> Exp.fresh, [Deferral(InAp) |> Exp.fresh])
      |> Exp.fresh,
      "x(_)",
    ),
    full_parser_test(
      "Cons",
      Cons(Int(1) |> Exp.fresh, ListLit([]) |> Exp.fresh) |> Exp.fresh,
      "1 :: []",
    ),
    full_parser_test(
      "ListLit",
      ListLit([
        Int(1) |> Exp.fresh,
        Int(2) |> Exp.fresh,
        Int(3) |> Exp.fresh,
      ])
      |> Exp.fresh,
      "[1, 2, 3]",
    ),
    menhir_only_test("Unit", Tuple([]) |> Exp.fresh, "()"),
    menhir_only_test(
      "Constructor",
      Constructor("A", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
      "A",
    ),
    menhir_only_test(
      "Constructor cast",
      Cast(
        Constructor("A", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
        Unknown(Internal) |> Typ.fresh,
        Int |> Typ.fresh,
      )
      |> Exp.fresh,
      "A : Int",
    ),
    menhir_only_test(
      "Constructor of specific sum type",
      Constructor("A", Int |> Typ.fresh) |> Exp.fresh,
      "A ~ Int",
    ),
    // TODO Fix for the tests below
    menhir_only_test(
      "Constructor with Type Variable",
      Constructor("A", Var("T") |> Typ.fresh) |> Exp.fresh,
      "A ~ T",
    ),
    full_parser_test(
      "Type Variable",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Var("T") |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        EmptyHole |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : T = ? in x",
    ),
    full_parser_test(
      "Type Alias",
      TyAlias(Var("x") |> TPat.fresh, Int |> Typ.fresh, Int(1) |> Exp.fresh)
      |> Exp.fresh,
      "type x = Int in 1",
    ),
    full_parser_test(
      "Test",
      Test(
        BinOp(Int(Equals), Int(3) |> Exp.fresh, Int(3) |> Exp.fresh)
        |> Exp.fresh,
      )
      |> Exp.fresh,
      "test 3 == 3 end",
    ),
    full_parser_test(
      "Filter",
      Filter(
        Filter({act: (Eval, All), pat: Int(3) |> Exp.fresh}),
        Int(3) |> Exp.fresh,
      )
      |> Exp.fresh,
      "eval 3 in 3" // TODO Use other filter commands
    ),
    full_parser_test(
      "List Concat",
      ListConcat(
        ListLit([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
        ListLit([Int(3) |> Exp.fresh, Int(4) |> Exp.fresh]) |> Exp.fresh,
      )
      |> Exp.fresh,
      "[1, 2] @ [3, 4]",
    ),
    full_parser_test(
      "times and divide precendence",
      BinOp(
        Int(Divide),
        BinOp(Int(Times), Int(1) |> Exp.fresh, Int(2) |> Exp.fresh)
        |> Exp.fresh,
        Int(3) |> Exp.fresh,
      )
      |> Exp.fresh,
      "1 * 2 / 3",
    ),
    full_parser_test(
      "plus and minus precendence",
      BinOp(
        Int(Plus),
        BinOp(Int(Minus), Int(1) |> Exp.fresh, Int(2) |> Exp.fresh)
        |> Exp.fresh,
        Int(3) |> Exp.fresh,
      )
      |> Exp.fresh,
      "1 - 2 + 3",
    ),
    full_parser_test(
      "Integer Ops",
      BinOp(
        Int(GreaterThanOrEqual),
        BinOp(
          Int(Minus),
          BinOp(
            Int(Plus),
            UnOp(Int(Minus), Int(1) |> Exp.fresh) |> Exp.fresh,
            Int(2) |> Exp.fresh,
          )
          |> Exp.fresh,
          BinOp(
            Int(Times),
            BinOp(Int(Divide), Int(3) |> Exp.fresh, Int(4) |> Exp.fresh)
            |> Exp.fresh,
            BinOp(Int(Power), Int(5) |> Exp.fresh, Int(6) |> Exp.fresh)
            |> Exp.fresh,
          )
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Int(8) |> Exp.fresh,
      )
      |> Exp.fresh,
      "-1 + 2 - 3 / 4 * 5 ** 6 >= 8",
    ),
    full_parser_test("Float", Float(1.) |> Exp.fresh, "1."),
    full_parser_test(
      "Float Ops",
      BinOp(
        Float(LessThan),
        BinOp(
          Float(Minus),
          Float(2.) |> Exp.fresh,
          BinOp(
            Float(Times),
            BinOp(
              Float(Divide),
              Float(3.) |> Exp.fresh,
              Float(4.) |> Exp.fresh,
            )
            |> Exp.fresh,
            BinOp(
              Float(Power),
              Float(5.) |> Exp.fresh,
              Float(6.) |> Exp.fresh,
            )
            |> Exp.fresh,
          )
          |> Exp.fresh,
        )
        |> Exp.fresh,
        Float(8.) |> Exp.fresh,
      )
      |> Exp.fresh,
      "2. -. 3. /. 4. *. 5. **. 6. <. 8.",
    ),
    full_parser_test(
      "Let binding with type ascription",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Int |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Int(5) |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let (x: Int) = 5 in x",
    ),
    menhir_only_test(
      "named_function",
      Fun(
        (Var("x"): Pat.term) |> Pat.fresh,
        BinOp(Int(Plus), Var("x") |> Exp.fresh, Int(5) |> Exp.fresh)
        |> Exp.fresh,
        None,
        Some("f"),
      )
      |> Exp.fresh,
      "named_fun f x -> x + 5",
    ),
    full_parser_test(
      "basic sum type",
      Let(
        Cast(
          Var("x") |> Pat.fresh,
          Sum([
            Variant("A", [], None),
            Variant("B", [], None),
            Variant("C", [], Some(Int |> Typ.fresh)),
          ])
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        Ap(
          Forward,
          Constructor("C", Unknown(Internal) |> Typ.fresh) |> Exp.fresh,
          Int(7) |> Exp.fresh,
        )
        |> Exp.fresh,
        Var("x") |> Exp.fresh,
      )
      |> Exp.fresh,
      "let x : +A +B +C(Int) = C(7) in x",
    ),
    menhir_maketerm_equivalent_test("Empty Type Hole", "let g: ? = 7 in g"),
    menhir_maketerm_equivalent_test(
      "Pattern with type ascription",
      "fun (b : Bool) -> b",
    ),
    full_parser_test(
      "Type Hole in arrow cast",
      Fun(
        Cast(
          Var("b") |> Pat.fresh,
          Parens(
            Arrow(
              Unknown(Hole(EmptyHole)) |> Typ.fresh,
              Unknown(Hole(EmptyHole)) |> Typ.fresh,
            )
            |> Typ.fresh,
          )
          |> Typ.fresh,
          Unknown(Internal) |> Typ.fresh,
        )
        |> Pat.fresh,
        EmptyHole |> Exp.fresh,
        None,
        None,
      )
      |> Exp.fresh,
      "fun (b : ? -> ?) -> ?",
    ),
    full_parser_test(
      "multiargument function",
      Ap(
        Forward,
        Var("f") |> Exp.fresh,
        Tuple([Int(1) |> Exp.fresh, Int(2) |> Exp.fresh]) |> Exp.fresh,
      )
      |> Exp.fresh,
      "f(1, 2)",
    ),
    menhir_maketerm_equivalent_test(
      "partial sum type",
      "type Partial = +Ok(?) + ? in ?",
    ),
    menhir_maketerm_equivalent_test(
      "Function with type variable",
      "fun (x : a) -> x",
    ),
    menhir_maketerm_equivalent_test("Sequence addition precedence", "1+2;3"),
    menhir_maketerm_equivalent_test(
      "And app precedence",
      "exp_equal(e1, e3) && exp_equal(e2, e4)",
    ),
    menhir_maketerm_equivalent_test(
      "Negation precedence with multiplication",
      "-num*1",
    ),
    menhir_maketerm_equivalent_test(
      "Concatenation association",
      "1::2::3::[]",
    ),
    menhir_maketerm_equivalent_test(
      "and less than precedence",
      "true && 23 < int_of_float(51.00)" // TODO This looks like a bug in MakeTerm
    ),
    menhir_maketerm_equivalent_test(
      ~speed_level=`Slow,
      "Altered Documentation Buffer: Basic Reference",
      {|
let empty_hole = ? in

let non_empty_hole : Int = true in

let bool: Bool = true in
let operators = !true && false || true in
let conditional = if !true then 1 else 2 in

let num: Int = 1 in
let arithmetic = -num*1 + 2/3 - 4**5 in
let comparison =
  (0 == 0, 0 < 1, 1 <= 1, 2 > 1, 1 >= 1)
in

let float: Float = 0.1 in
let artihmetic = 0. *. 1. +. 2. /. 3. -. 4. **. 5. in
let comparison =
  (0. ==. 0., 0. <. 1., 1. <=. 1., 2. >. 1., 1. >=. 1.)
in

let string = "Hello, world!" in
let concatenation  = string ++ " Goodbye." in
let comparison = string$== "Hello, world!" in

let tuple : (Int, Bool, (Bool, Int)) =
(1, true, (false, 3)) in
let (a, b, (c, d)) = tuple in

let y : (Int, Int, Int) -> Int =
fun (m, x, b) -> m * x + b in

let double_recursively : Int -> Int =
  fun n ->
    if n == 0
    then 0
    else double_recursively(n - 1) + 2
in

let (even : Int -> Bool, odd : Int -> Bool) =
  (fun n -> if n == 0 then true else odd(n - 1),
  fun n -> if n == 0 then false else even(n - 1))
in

let empty_list : [Int] = [] in
let non_empty_list : [Int] = 1::2::3::[] in
let list_literals : [Int] = [1, 2, 3] in
let length : [Int] -> Int =
  fun xs ->
    case xs
      | [] => 0
      | hd::tl => 1 + length(tl)
    end
in
let has_at_least_two_elements : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | hd::[] => false
      | a::b::[] => true
    end
in

type Exp =
  + Var(String)
  + Lam(String, Exp)
+ Ap(Exp, Exp) in
let exp_equal: (Exp, Exp) -> Bool =
  fun es ->
    case es
      | (Var(x), Var(y)) => x$== y
      | (Lam((x1, e1)), Lam((x2, e2))) => x1$== x2 && exp_equal(e1, e2)
      | (Ap((e1, e2)), Ap((e3, e4))) => exp_equal(e1, e3) && exp_equal(e2, e4)
      | _ => false
    end
in

let poly_id: (forall a -> (a -> a)) =
  (typfun a -> (fun (x : a) -> x))
in
let apply_both:
forall a -> forall b -> (forall c -> c -> c) -> ((a, b) -> (a, b)) =
  typfun a -> typfun b ->
    fun (f : forall c -> (c -> c)) ->
      fun ((x, y) : (a, b)) -> (f@<a>(x), f@<b>(y))
in
let list_length: forall a -> ([a] -> Int) =
  typfun a -> fun (l : [a]) ->
    case l
      | [] => 0
      | hd::tl => 1 + list_length@<a>(tl)
    end
in

test 2 + 2 == 4 end;
test 3 + 3 == 6 end;
test 2 + 2 == 5 end;

2 + 2
    |},
    ),
    menhir_maketerm_equivalent_test(
      ~speed_level=`Slow,
      "Altered Documentation Buffer: Projectors",
      {|
let fold = (((((((((((()))))))))))) in
let folds: (Int -> Bool) = ? in
let guard: Bool = true in
let phase: Int = 44 in
let float: Float = 79.00 in
let (a:Int, f: Float) = (true, 28) in
let _ = "" in
let __ = "" in
let ___ = "a" in
let ____ = "shift" in
let _____ = "malicious" in
let ______ = "a shift   malicious" in
let box: Int = "malicious" in
if true && (23 < int_of_float(51.00))
then ______ else "its: " ++ box    |},
    ),
    menhir_maketerm_equivalent_test(
      ~speed_level=`Slow,
      "Altered Documentation Buffer: Types & Static Errors",
      {|
let _ = unbound in
let Undefined = Undefined in
let true = 2 in

let ? = if true then 1 else 1. in
let _ = if true then 1 else 1. in
let _: ? = if true then 1 else 1. in
let _: Int = if true then 1 else 1. in
let _: Fake = if true then 1 else true in
let (_, _) = if true then 1 else 1. in
let (_, _) = ((if true then 1 else 1.),?)    in
let (_: ?, _) = ((if true then 1 else 1.),?)    in
let [_] = [(if true then 1 else 1.)] in
let [_] = (if true then 1 else 1.) in

(?)(if true then 1 else 1.);
1(if true then 1 else 1.);
(1)(if true then 1 else 1.);
(fun ? -> ?)(if true then 1 else 1.);
(fun _ -> ?)(if true then 1 else 1.);
(fun (_: ?) -> ?)(if true then 1 else 1.);
(fun (_: Int) -> ?)(if true then 1 else 1.);

let _ = fun x -> if true then 1 else 1. in
let _: ? = fun x -> if true then 1 else 1. in
let _: ? -> ?  = fun x -> if true then 1 else 1. in
let _: ? -> Int = fun x -> if true then 1 else 1. in
let _: ? -> [?] = fun x -> if true then 1 else 1. in

(?)::[(if true then 1 else 1.)];
1::[(if true then 1 else 1.)];
(1, 1)::[(if true then 1 else 1.)];

let ? = [1, 1., true] in
let _ = [1, 1., true] in
let _: ? = [1, 1., true] in
let _: [?] = [1, 1., true] in
let _: [Int] = [1, 1., true] in

let _: [Int] = 1::[2] in
let _: [Int] = 1.0::[2] in
let _: [Int] = 1::[2.0] in
"BYE"
|},
    ),
    menhir_maketerm_equivalent_test(
      ~speed_level=`Slow,
      "Altered Documentation Buffer: adt dynamics",
      {|
type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp) in

let exp_equal: (Exp, Exp) -> Bool =
  fun es ->
    case es
      | (Var(x), Var(y)) => x$== y
      | (Lam((x1, e1)), Lam((x2, e2))) => x1$== x2 && exp_equal(e1, e2)
      | (Ap((e1, e2)), Ap((e3, e4))) => exp_equal(e1, e3) && exp_equal(e2, e4)
      | _ => false end in

let subst: (Exp, String, Exp) -> Exp=
  fun (v, name, f) ->
    case f
      | Var(n) =>
        (if n$== name then v else f)
      | Lam((x, body)) =>
        Lam(x, subst(v,name, body))
      | Ap((e1,e2)) =>
  Ap(subst(v, name, e1), subst(v, name, e2)) end in

type Result =
  + Error(String)
  + Ok(Exp)
in

let result_equal: (Result, Result) -> Bool =
  fun rs ->
    case rs
      | (Ok(e1), Ok(e2)) => exp_equal(e1, e2)
      | (Error(e1), Error(e2)) => e1$== e2
| _ => false end in

let go: Exp -> Result =
  fun f ->
    case f
      | Var(n) => Error("Free Variable")
      | Lam((x, body)) => Ok(Lam(x, body))
      | Ap((e1,e2)) =>
      case go(e1)
        | Ok(Lam((x, body)))=>
        case go(e2)
          | Error(err) => Error(err)
        | Ok(arg) => go(subst(arg, x, body)) end
| _ => Error("Not a Function") end end in

test result_equal(
  go(Var("yo")),
Error("Free Variable")) end;

test result_equal(
  go(Ap(Var("no"), Lam("bro", Var("bro")))),
Error("Not a Function")) end;

test result_equal(
  go(Lam("yo", Var("yo"))),
Ok(Lam("yo", Var("yo")))) end;

test result_equal(
  go(Ap(Lam("yo", Var("yo")), Lam("bro", Var("bro")))),
Ok(Lam("bro", Var("bro")))) end
|},
    ),
    menhir_maketerm_equivalent_test(
      // Variable names are renamed due to lexing overtaking e, t, p, and tp
      ~speed_level=`Slow,
      "Altered Documentation Buffer: Polymorphism",
      {|let id = typfun A -> (fun (x : A) -> x) in
let ex1 = id@<Int>(1) in
let const : forall A -> (forall B -> (A -> B -> A)) =
typfun A -> (typfun B -> (fun x -> fun y -> x)) in
let ex2 = const@<Int>@<String>(2)("Hello World") in
let apply_both : forall A -> forall B -> (forall D -> D -> D) -> (A , B) -> (A , B) =
typfun A -> typfun B -> fun f -> fun (x, y) -> (f@<A>(x), f@<B>(y)) in
let ex3 = apply_both@<Int>@<String>(id)(3, "Hello World") in
let emptylist : forall A -> [A] = typfun A -> [] in
let map : forall A -> forall B -> (A -> B) -> ([A] -> [B]) =
  typfun A -> typfun B -> fun (f : (A -> B)) -> fun (l : [A]) ->
    case l
      | (h :: a) => f(h) :: map@<A>@<B>(f)(a)
      | _ => emptylist@<B>
end in
let ex4 = map@<Int>@<String>(string_of_int)([1,2,3]) in
type MyList = rec A -> (+Nil + Cons(Int, A)) in
let x : MyList = Cons(1, Cons(2, Cons(3, Nil))) in
type MyList2 = +Nil + Cons(Int, MyList2) in
type Broken = Int -> (+HasInt(Int) + HasMore(Int, Broken)) in
let list_of_mylist : (MyList -> [Int]) = fun (myl : MyList) ->
  case myl
    | Nil => []
    | Cons((h, a)) => h :: list_of_mylist(a)
end in
let ex5 = list_of_mylist(x) in
(ex1, ex2, ex3, ex4, ex5)
    |},
    ),
    // This fails because MakeTerm can't handle left to right keyword prefixes.
    skip_menhir_maketerm_equivalent_test(
      "Prefixed keyword parses",
      {|let ? = ina in ?|},
    ),
    skip_menhir_maketerm_equivalent_test(
      "Sum type messed up in make term",
      {|type ? = rec ? -> + Aramj -> Bool in ?|},
    ),
    skip_menhir_maketerm_equivalent_test(
      "List concat and typap",
      {|type ? = (+ Ulog, () -> Float) in let (()) = (()) in 0.001536|},
    ),
    skip_menhir_maketerm_equivalent_test(
      "Sum in product in typeap",
      {|((fun _ -> b)) @< [(+ Kfgii, Float)] >|},
    ),
    skip_menhir_maketerm_equivalent_test(
      "Non-unique constructors currently throws in equality",
      {|type ? = ((+ ? + ?)) in []|},
    ),
    QCheck_alcotest.to_alcotest(qcheck_menhir_maketerm_equivalent_test),
    // Disabled due to bugs in ExpToSegment
    QCheck_alcotest.to_alcotest(qcheck_menhir_serialized_equivalent_test),
  ],
);

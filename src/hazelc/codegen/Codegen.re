let sprintf = Printf.sprintf;

module type ImportMeta = {
  let path: string;
  let name: string;
};

module Import = (M: ImportMeta) => {
  let code = (): GHExp.top_block => [Import(M.name, M.path)];

  let symbol = symbol => GHExp.Named(sprintf("%s.%s", M.name, symbol));
};

module Consts = {
  let truelit = "true";
  let falselit = "false";
  let voidlit = "void";

  let prim_and_op = "&&";
  let prim_or_op = "||";
  let prim_plus_op = "+";
  let prim_minus_op = "-";
  let prim_times_op = "*";
  let prim_divide_op = "/";
  let prim_less_than_op = "<";
  let prim_greater_than_op = "<";
  let prim_equals_op = "<";
};

module Pervasives = {
  let inj_left = "L";
  let inj_right = "R";

  let code = (): GHExp.top_block => [
    Decl(
      Enum(
        "HazelSum",
        ["a", "b"],
        [(inj_right, ["a"]), (inj_right, ["b"])],
      ),
    ),
  ];
};

module HazelMod = {
  module Mod =
    Import({
      let path = "hazel/hazel";
      let name = "Hazel";
    });

  let code = Mod.code;

  let hazel_and = Mod.symbol("and");
  let hazel_or = Mod.symbol("or");

  let hazel_plus = Mod.symbol("plus");
  let hazel_minus = Mod.symbol("minus");
  let hazel_times = Mod.symbol("times");
  let hazel_divide = Mod.symbol("divide");
  let hazel_less_than = Mod.symbol("less_than");
  let hazel_greater_than = Mod.symbol("greater_than");
  let hazel_equals = Mod.symbol("equals");

  let hazel_fplus = Mod.symbol("fplus");
  let hazel_fminus = Mod.symbol("fminus");
  let hazel_ftimes = Mod.symbol("ftimes");
  let hazel_fdivide = Mod.symbol("fdivide");
  let hazel_fless_than = Mod.symbol("fless_than");
  let hazel_fgreater_than = Mod.symbol("fgreater_than");
  let hazel_fequals = Mod.symbol("fequals");
};

module BuiltinsMod = {
  module Mod =
    Import({
      let path = "hazel/builtins";
      let name = "Builtins";
    });

  let code = Mod.code;

  let builtins =
    [
      ("int_of_float", "intOfFloat"),
      ("float_of_int", "floatOfInt"),
      ("PI", "pi"),
      ("mod", "mod"),
    ]
    |> List.map(((name, libname)) => (name, Mod.symbol(libname)));

  let lookup = VarMap.lookup(builtins);
};

module Preamble = {
  let code = () =>
    [Pervasives.code(), HazelMod.code(), BuiltinsMod.code()]
    |> GHExp.TopBlock.join;
};

/*
   Raise a CodegenError.Exception.
 */
let codegen_raise = err => raise(CodegenError.Exception(err));

let codegen_infix = (o1, op, o2) => sprintf("%s %s %s", o1, op, o2);
let codegen_lines = ss => ss |> String.concat("\n");
let codegen_comma_sep = ss => ss |> String.concat(", ");

let rec codegen = ((tb, b): GHExp.program) => {
  let preamble = GHExp.TopBlock.join([Preamble.code(), tb]);

  [codegen_top_block(preamble), codegen_block_nowrap(b)] |> codegen_lines;
}

and codegen_top_block = (tb: GHExp.top_block) => {
  tb |> List.map(codegen_top_statement) |> codegen_lines;
}

and codegen_top_statement = (tstmt: GHExp.top_statement) => {
  switch (tstmt) {
  | Import(name, path) => codegen_import(name, path)
  | Decl(decl) => codegen_decl(decl)
  };
}

and codegen_import = (name: Var.t, path: string) =>
  // TODO: Relative import?
  sprintf("import %s from \"%s\"", name, path)

and codegen_decl = (decl: GHExp.decl) => {
  switch (decl) {
  | Enum(name, type_vars, variants) =>
    let type_vars =
      List.length(type_vars) == 0
        ? "" : type_vars |> codegen_comma_sep |> sprintf("<%s>");
    let variants =
      variants
      |> List.map(((vname, vbody)) =>
           if (List.length(vbody) == 0) {
             vname;
           } else {
             vbody |> codegen_comma_sep |> sprintf("%s(%s)", vname);
           }
         )
      |> codegen_comma_sep;

    sprintf("enum %s%s { %s }", name, type_vars, variants);
  };
}

and codegen_block_nowrap = (b: GHExp.block) => {
  b |> List.map(codegen_statement) |> codegen_lines;
}
and codegen_block = (b: GHExp.block) => {
  let s = codegen_block_nowrap(b);
  sprintf("{ %s }", s);
}

and codegen_statement = (stmt: GHExp.statement) =>
  switch (stmt) {
  | Let(params, e) =>
    let params = codegen_params(params);
    let e = codegen_expr(e);
    sprintf("let %s = %s", params, e);
  | LetRec(params, e) =>
    let params = codegen_params(params);
    let e = codegen_expr(e);
    sprintf("let rec %s = %s", params, e);
  | Expr(e) => codegen_expr(e)
  }

and codegen_expr = (e: GHExp.expr) =>
  switch (e) {
  | BoolLit(b) => b ? Consts.truelit : Consts.falselit
  | IntLit(n) => string_of_int(n)
  // TODO: NaN?
  | FloatLit(f) => string_of_float(f)
  | BinOp(op, e1, e2) => codegen_bin_op(op, e1, e2)
  | List(es) =>
    let es = es |> List.map(codegen_expr) |> String.concat(", ");
    sprintf("[%s]", es);
  | Triv => Consts.voidlit
  | Var(var) => codegen_var(var)
  | Builtin(name) =>
    let builtin = BuiltinsMod.lookup(name);
    switch (builtin) {
    | Some(builtin) => codegen_expr(Var(builtin))
    | None => codegen_raise(BadBuiltin(name))
    };
  | Inj(side, e') => codegen_inj(side, e')
  | Lam(_params, _e') => codegen_raise(NotImplemented)
  | Ap(_lam, _args) => codegen_raise(NotImplemented)
  | Match(_e', _rules) => codegen_raise(NotImplemented)
  | Block(b) => codegen_block(b)
  }

and codegen_bin_op = (op: GHExp.bin_op, e1: GHExp.expr, e2: GHExp.expr) =>
  switch (op) {
  | Prim(op') => codegen_bin_op_prim(op', e1, e2)
  | Hazel(op') => codegen_bin_op_hazel(op', e1, e2)
  }
and codegen_bin_op_prim = (op: GHExp.BinOp.op, e1: GHExp.expr, e2: GHExp.expr) => {
  let op =
    switch (op) {
    | And => Consts.prim_and_op
    | Or => Consts.prim_or_op
    | Plus => Consts.prim_plus_op
    | Minus => Consts.prim_minus_op
    | Times => Consts.prim_times_op
    | Divide => Consts.prim_divide_op
    | LessThan => Consts.prim_less_than_op
    | GreaterThan => Consts.prim_greater_than_op
    | Equals => Consts.prim_equals_op
    | FPlus => Consts.prim_plus_op
    | FMinus => Consts.prim_minus_op
    | FTimes => Consts.prim_times_op
    | FDivide => Consts.prim_divide_op
    | FLessThan => Consts.prim_less_than_op
    | FGreaterThan => Consts.prim_greater_than_op
    | FEquals => Consts.prim_equals_op
    };
  let e1 = codegen_expr(e1);
  let e2 = codegen_expr(e2);
  sprintf("%s %s %s", e1, op, e2);
}
and codegen_bin_op_hazel =
    (op: GHExp.BinOp.op, e1: GHExp.expr, e2: GHExp.expr) => {
  let f =
    switch (op) {
    | And => HazelMod.hazel_and
    | Or => HazelMod.hazel_or
    | Plus => HazelMod.hazel_plus
    | Minus => HazelMod.hazel_minus
    | Times => HazelMod.hazel_times
    | Divide => HazelMod.hazel_divide
    | LessThan => HazelMod.hazel_less_than
    | GreaterThan => HazelMod.hazel_greater_than
    | Equals => HazelMod.hazel_equals
    | FPlus => HazelMod.hazel_fplus
    | FMinus => HazelMod.hazel_fminus
    | FTimes => HazelMod.hazel_ftimes
    | FDivide => HazelMod.hazel_fdivide
    | FLessThan => HazelMod.hazel_fless_than
    | FGreaterThan => HazelMod.hazel_fgreater_than
    | FEquals => HazelMod.hazel_fequals
    };
  codegen_expr(Ap(Var(f), [e1, e2]));
}

and codegen_inj = (side: GHExp.side, e': GHExp.expr) => {
  let ctor =
    switch (side) {
    | L => Pervasives.inj_left
    | R => Pervasives.inj_right
    };

  // TODO: Use separate expr variant for variant constructor?
  codegen_expr(Ap(Var(Named(ctor)), [e']));
}

and codegen_var = (var: GHExp.var) =>
  switch (var) {
  | Named(var) => var
  | Tmp(n) => sprintf("t%d", n)
  }
and codegen_params = (ps: GHExp.params) =>
  ps |> List.map(codegen_var) |> String.concat(", ");

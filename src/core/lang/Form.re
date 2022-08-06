open Sexplib.Std;
open Mold;
module P = Precedence;

/* FORM

   This module determines the syntactic extent of the language; the
   entire Syntax module is driven by the below definitions. Adding
   a new syntactic form is simply a matter of adding a new line to either
   the 'convex_monos' table, for single-token forms, or the 'forms'
   table, for compound forms.

   The wrapping functions seen in both of those tables determine the
   shape, precedence, and expansion behavior of the form. */

let regexp = (r, s) => Re.Str.string_match(Re.Str.regexp(r), s, 0);

/* A label is the textual expression of a form's delimiters */
[@deriving (show({with_path: false}), sexp, yojson)]
type label = list(Token.t);

/* The construction of a compound forms can be triggered by inserting
   one of its delimiters through a process called expansion. Expansion
   can either occur (Instant)ly upon delimiter creation, or be (Delayed)
   until after a token boundary event is triggered (say by pressing
   space after entering 'let'). The (Static) case is used for monos
   aka single-token forms. */
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion_time =
  | Static
  | Instant
  | Delayed;

/* Expansion can be triggered by either/both the first or last token
   of a form, represented here by the first/last elements of this pair. */
[@deriving (show({with_path: false}), sexp, yojson)]
type expansion = (expansion_time, expansion_time);

/* A label, a mold, and expansion behavior together determine a form. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  label,
  expansion,
  mold: Mold.t,
};

let mk = (expansion, label, mold) => {label, mold, expansion};

/* Abbreviations for expansion behaviors */
let ss: expansion = (Static, Static);
let ii: expansion = (Instant, Instant);
let id: expansion = (Instant, Delayed);
let is: expansion = (Instant, Static);
let ds: expansion = (Delayed, Static);
let di: expansion = (Delayed, Instant);

let mk_infix = (t: Token.t, sort: Sort.t, prec) =>
  mk(ss, [t], mk_bin(prec, sort, []));

/* Regular expressions which determine monotiles */
let is_var = regexp("^[a-z][A-Za-z0-9_]*$");
let is_int = regexp("^[0-9]*$");
let is_float = regexp("^[0-9]*\\.[0-9]*$");
let is_typ_lit = regexp("^Int|Float|Bool$");
let is_typ_lit_partial = regexp("^[A-Z][A-Za-z0-9_]*$");
let is_wild = regexp("^_$");
let is_bool = regexp("^true|false$");

/* A. Whitespace: */
let whitespace = [Whitespace.space, Whitespace.linebreak];

/* B. Operands:
   Order in this list determines relative remolding
   priority for forms with overlapping regexps */
let convex_monos: list((string, (string => bool, list(Mold.t)))) = [
  ("var", (is_var, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("type", (is_typ_lit, [mk_op(Exp, [])])), // bad sort
  ("type-partial", (is_typ_lit_partial, [mk_op(Typ, [])])), // bad sort (should be bottom)
  // TODO(andrew): above thing: color same as sort inconsistency errors
  // ("whatever", (regexp("#*$"), [mk_op(Nul, [])])),
  // ("whatever", (regexp("@*$"), [mk_op(Rul, [])])),
  ("fnum", (is_float, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("num", (is_int, [mk_op(Exp, []), mk_op(Pat, [])])),
  ("wild", (is_wild, [mk_op(Pat, [])])),
];

/* C. Compound Forms:
   Order in this list determines relative remolding
   priority for forms which share the same labels */
let forms: list((string, t)) = [
  ("cell-join", mk_infix(";", Exp, 10)),
  ("plus", mk_infix("+", Exp, P.plus)),
  ("minus", mk_infix("-", Exp, P.plus)),
  ("fplus", mk_infix("+.", Exp, P.plus)),
  ("equals", mk_infix("=", Exp, P.eqs)),
  ("lt", mk_infix("<", Exp, P.eqs)),
  ("gt", mk_infix(">", Exp, P.eqs)),
  ("flt", mk_infix("<.", Exp, P.eqs)),
  ("bitwise_and", mk_infix("&", Exp, 5)), // substring req
  ("logical_and", mk_infix("&&", Exp, 5)),
  //("times", mk_infix("*", Exp, P.mult)),
  //("divide", mk_infix("/", Exp, P.mult)),
  //("not_equals", mk_infix("!=", Exp, 5)),
  //("gt", mk_infix(">", Exp, P.eqs)),
  //("gte", mk_infix("<=", Exp, P.eqs)),
  //("lte", mk_infix(">=", Exp, P.eqs)),
  //("bitwise_or", mk_infix("|", Exp, 5)),
  //("logical_or", mk_infix("||", Exp, 5)),
  ("type-arrow", mk_infix("->", Typ, 6)), // bad sorts
  //("unary_minus", mk(ss, ["-"], mk_pre(P.fact, Exp, []))), // substring req
  ("comma_exp", mk_infix(",", Exp, P.prod)),
  ("comma_pat", mk_infix(",", Pat, P.prod)),
  ("comma_typ", mk_infix(",", Typ, P.prod)),
  ("parens_exp", mk(ii, ["(", ")"], mk_op(Exp, [Exp]))), // construction req for ap
  ("parens_pat", mk(ii, ["(", ")"], mk_op(Pat, [Pat]))),
  (
    "funann",
    mk(ds, ["funann", ":", "->"], mk_pre(P.let_, Exp, [Pat, Typ])),
  ),
  ("fun_", mk(ds, ["fun", "->"], mk_pre(P.let_, Exp, [Pat]))),
  ("if_", mk(di, ["if", "then", "else"], mk_pre(P.if_, Exp, [Exp, Exp]))),
  ("ap", mk(ii, ["(", ")"], mk_post(P.ap, Exp, [Exp]))),
  ("let_", mk(ds, ["let", "=", "in"], mk_pre(P.let_, Exp, [Pat, Exp]))),
  (
    "letann",
    mk(
      ds,
      ["letann", ":", "=", "in"],
      mk_pre(P.let_, Exp, [Pat, Typ, Exp]),
    ),
  ),
  ("typeann", mk(ss, [":"], mk_bin'(P.ann, Pat, Pat, [], Typ))),
  (
    "case",
    mk(ds, ["case", "of"], mk_pre'(P.case_, Exp, Exp, [Exp], Rul)),
  ),
  ("rule_arr", mk(ss, ["=>"], mk_bin'(P.rule_arr, Rul, Pat, [], Exp))),
  ("rule_sep", mk_infix("|", Rul, P.rule_sep)),
  ("test", mk(ds, ["test", "end"], mk_op(Exp, [Exp]))),
  //("concat", mk_infix("@", Exp, P.concat)),
  //("rev_ap", mk_infix("|>", Exp, P.eqs)),
  //("cons", mk_infix("::", Exp, 5)),
  //("type-ann", mk_infix(":", Exp, 5)), // bad sorts
  //("dot-access", mk_infix(".", Exp, 5)), // bad sorts
  //("assign_incr", mk_infix("+=", Exp, 10)), // bad sorts
  //("semi", mk_infix(";", Exp, P.semi)),
  //("fact", mk(ss, ["!"], mk_post(P.fact, Exp, []))),
  // ("array_access", mk(ii, ["[", "]"], mk_post(P.ap, Exp, [Exp]))),
  //("list_lit", mk(ii, ["[", "]"], mk_op(Exp, [Exp]))),
  //("cond", mk(is, ["?", ":"], mk_bin(P.cond, Exp, [Exp]))),
  //("block", mk(ii, ["{", "}"], mk_op(Exp, [Exp]))),
  //("case", mk(ds, ["case", "of"], mk_pre(9, Exp, [Exp]))),
  //("rule_first", mk(ds, ["|", "->"], mk_pre(9, Exp, [Pat]))),
  /* Something must instant on | as not valid monotile on its own */
  //("rule_rest", mk(ds, ["|", "->"], mk_bin(9, Exp, [Pat]))),
];

let get: String.t => t = name => List.assoc(name, forms);

let delims: list(Token.t) =
  forms
  |> List.fold_left((acc, (_, {label, _}: t)) => {label @ acc}, [])
  |> List.sort_uniq(compare);

let convex_mono_molds: Token.t => list(Mold.t) =
  s =>
    List.fold_left(
      (acc, (_, (test, molds))) => test(s) ? molds @ acc : acc,
      [],
      convex_monos,
    );

let is_convex_mono = t => convex_mono_molds(t) != [];
let is_whitespace = t => List.mem(t, whitespace);
let is_delim = t => List.mem(t, delims);

let is_valid_token = t =>
  is_convex_mono(t) || is_whitespace(t) || is_delim(t);

let is_valid_char = is_valid_token; //TODO(andrew): betterify this

let mk_convex_mono = (sort: Sort.t, t: Token.t) => {
  assert(is_convex_mono(t));
  mk(ss, [t], Mold.(mk_op(sort, [])));
};

module DrvSort = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Jdmt
    | Prop
    | Exp
    | Pat
    | Typ
    | TPat;

  let all = [Jdmt, Prop, Exp, Pat, Typ, TPat];

  let to_string =
    fun
    | Jdmt => "Jdmt"
    | Prop => "Prop"
    | Exp => "ALFA_Exp"
    | Pat => "ALFA_Pat"
    | Typ => "ALFA_Typ"
    | TPat => "ALFA_TPat";

  let to_string_verbose =
    fun
    | Jdmt => "judgement"
    | Prop => "proposition"
    | Exp => "ALFA expression"
    | Pat => "ALFA pattern"
    | Typ => "ALFA type"
    | TPat => "ALFA type pattern";
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Drv(DrvSort.t)
  | Any
  | Nul
  | Pat
  | Typ
  | TPat
  | Rul
  | Exp;

let root = Exp;

let all =
  (DrvSort.all |> List.map(s => Drv(s)))
  @ [Any, Nul, Pat, Typ, Rul, Exp, TPat];

let consistent = (s, s') =>
  switch (s, s') {
  | (Any, _)
  | (_, Any) => true
  | (Nul, _)
  | (_, Nul) => false
  | _ => s == s'
  };

let to_string =
  fun
  | Drv(s) => DrvSort.to_string(s)
  | Any => "Any"
  | Nul => "Nul"
  | Pat => "Pat"
  | TPat => "TPat"
  | Typ => "Typ"
  | Rul => "Rul"
  | Exp => "Exp";

let to_string_verbose =
  fun
  | Drv(s) => DrvSort.to_string_verbose(s)
  | Any => "any"
  | Nul => "null"
  | Pat => "pattern"
  | TPat => "type pattern"
  | Typ => "type"
  | Rul => "rule"
  | Exp => "expression";

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  // Gradual Typing
  // - Type consistency
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow
  // - Type Matched
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow
  // - Gradual Typing Rules
  | S_Hole
  | A_Subsumption_GT
  | S_If_GT
  | S_PrjL_GT
  | S_PrjR_GT
  | A_Pair_GT
  | S_LetPair_GT
  | A_LetPair_GT
  | A_InjL_GT
  | A_InjR_GT
  | A_Case_GT
  | S_Case_GT
  | A_FunAnn_GT
  | A_Fun_GT
  | S_Ap_GT
  // Type Validity
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar
  // Typing
  | A_Subsumption
  // - Booleans
  | T_True
  | S_True
  | T_False
  | S_False
  | T_If
  | S_If
  | A_If
  // - Numbers
  | T_Num
  | S_Num
  | T_Neg
  | S_Neg
  | T_Plus
  | S_Plus
  | T_Minus
  | S_Minus
  | T_Times
  | S_Times
  | T_Lt
  | S_Lt
  | T_Gt
  | S_Gt
  | T_Eq
  | S_Eq
  // - Variables
  | T_Var
  | S_Var
  | T_LetAnn
  | T_LetAnn_TV
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  // - Functions
  | T_FunAnn
  | T_FunAnn_TV
  | S_FunAnn
  | A_FunAnn
  | T_Fun
  | A_Fun
  | T_Ap
  | S_Ap
  // - Products
  | T_Triv
  | S_Triv
  | T_Pair
  | S_Pair
  | A_Pair
  | T_LetPair
  | S_LetPair
  | A_LetPair
  | T_PrjL
  | S_PrjL
  | T_PrjR
  | S_PrjR
  // - Sums
  | T_InjL
  | A_InjL
  | T_InjR
  | A_InjR
  | T_Case
  | S_Case
  | A_Case
  // - Fixpoints
  | T_Fix
  | T_FixAnn
  | T_FixAnn_TV
  // - Recursive
  | T_Roll
  | T_Unroll
  // Values
  | V_Num
  | V_True
  | V_False
  | V_Fun
  | V_Pair
  | V_Triv
  | V_InjL
  | V_InjR
  | V_Roll
  // Evaluation
  // - Value Evaluation
  | E_Val
  // - Booleans
  | E_If_T
  | E_If_F
  // - Numbers
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  // - Variables
  | E_Let
  | E_Ap
  // - Products
  | E_Pair
  | E_PrjL
  | E_PrjR
  | E_LetPair
  // - Sums
  | E_InjL
  | E_InjR
  | E_Case_L
  | E_Case_R
  // - Fixpoints
  | E_Fix
  // - Recursive
  | E_Roll
  | E_Unroll
  // Propositional logic
  | Assumption
  | And_I
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E;

let show = rule => show(rule) |> String.map(c => c == '_' ? '-' : c);

let repr =
  fun
  // // Propositional logic
  | Assumption => "Asm."
  | And_I => "∧-I"
  | And_E_L => "∧-E-L"
  | And_E_R => "∧-E-R"
  | Or_I_L => "∨-I-L"
  | Or_I_R => "∨-I-R"
  | Or_E => "∨-E"
  | Implies_I => "⊃-I"
  | Implies_E => "⊃-E"
  | Truth_I => "⊤-I"
  | Falsity_E => "⊥-E"
  | A_Subsumption => "A-Sub."
  | rule => show(rule);

[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  | TypeConsistency
  | TypeMatched
  | TypeValidity
  | Synthesis
  | Analysis
  | Typing
  | Values
  | Evaluation
  | PropositionalLogic(prop_logic_kind)
and prop_logic_kind =
  | Assumption
  | Introduction
  | Elimination;

let of_kind: t => kind =
  fun
  // - Type consistency
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow => TypeConsistency
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow => TypeMatched
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar => TypeValidity
  | S_Hole
  | S_If_GT
  | S_PrjL_GT
  | S_PrjR_GT
  | S_LetPair_GT
  | S_Case_GT
  | S_Ap_GT
  | S_True
  | S_False
  | S_If
  | S_Num
  | S_Neg
  | S_Plus
  | S_Minus
  | S_Times
  | S_Lt
  | S_Gt
  | S_Eq
  | S_Var
  | S_LetAnn
  | S_Let
  | S_FunAnn
  | S_Ap
  | S_Triv
  | S_Pair
  | S_LetPair
  | S_PrjL
  | S_PrjR
  | S_Case => Synthesis
  | A_Subsumption_GT
  | A_Pair_GT
  | A_LetPair_GT
  | A_InjL_GT
  | A_InjR_GT
  | A_Case_GT
  | A_FunAnn_GT
  | A_Fun_GT
  | A_Subsumption
  | A_If
  | A_LetAnn
  | A_Let
  | A_FunAnn
  | A_Fun
  | A_Pair
  | A_LetPair
  | A_InjL
  | A_InjR
  | A_Case => Analysis
  | T_True
  | T_False
  | T_If
  | T_Num
  | T_Neg
  | T_Plus
  | T_Minus
  | T_Times
  | T_Lt
  | T_Gt
  | T_Eq
  | T_Var
  | T_LetAnn
  | T_LetAnn_TV
  | T_Let
  | T_FunAnn
  | T_FunAnn_TV
  | T_Fun
  | T_Ap
  | T_Triv
  | T_Pair
  | T_LetPair
  | T_PrjL
  | T_PrjR
  | T_InjL
  | T_InjR
  | T_Case
  | T_Fix
  | T_FixAnn
  | T_FixAnn_TV
  | T_Roll
  | T_Unroll => Typing
  | V_Num
  | V_True
  | V_False
  | V_Fun
  | V_Pair
  | V_Triv
  | V_InjL
  | V_InjR
  | V_Roll => Values
  | E_Val
  | E_If_T
  | E_If_F
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  | E_Let
  | E_Ap
  | E_Pair
  | E_PrjL
  | E_PrjR
  | E_LetPair
  | E_InjL
  | E_InjR
  | E_Case_L
  | E_Case_R
  | E_Fix
  | E_Roll
  | E_Unroll => Evaluation
  | Assumption => PropositionalLogic(Assumption)
  | And_I
  | Or_I_L
  | Or_I_R
  | Implies_I
  | Truth_I => PropositionalLogic(Introduction)
  | And_E_L
  | And_E_R
  | Or_E
  | Implies_E
  | Falsity_E => PropositionalLogic(Elimination);

[@deriving (show({with_path: false}), sexp, yojson)]
type sort =
  | TypeConsistency
  | TypeMatched
  | TypeValidity
  | A_Subsumption
  | E_Val
  | Boolean
  | Number
  | Variable
  | Function
  | Product
  | Sum
  | Fixpoint
  | Recursive
  | PropositionalLogic;

let of_sort: t => sort =
  fun
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow => TypeConsistency
  | S_Hole // Should be somewhere else
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow => TypeMatched
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar => TypeValidity
  | T_True
  | S_True
  | V_True
  | T_False
  | S_False
  | V_False
  | T_If
  | S_If
  | S_If_GT
  | A_If
  | E_If_T
  | E_If_F => Boolean
  | T_Num
  | S_Num
  | V_Num
  | T_Neg
  | S_Neg
  | E_Neg
  | T_Plus
  | S_Plus
  | E_Plus
  | T_Minus
  | S_Minus
  | E_Minus
  | T_Times
  | S_Times
  | E_Times
  | T_Lt
  | S_Lt
  | E_Lt_T
  | E_Lt_F
  | T_Gt
  | S_Gt
  | E_Gt_T
  | E_Gt_F
  | T_Eq
  | S_Eq
  | E_Eq_T
  | E_Eq_F => Number
  | T_Var
  | S_Var
  | T_LetAnn
  | T_LetAnn_TV
  | S_LetAnn
  | A_LetAnn
  | T_Let
  | S_Let
  | A_Let
  | E_Let => Variable
  | T_FunAnn
  | T_FunAnn_TV
  | S_FunAnn
  | A_FunAnn
  | A_FunAnn_GT
  | T_Fun
  | A_Fun
  | A_Fun_GT
  | V_Fun
  | T_Ap
  | S_Ap
  | S_Ap_GT
  | E_Ap => Function
  | T_Triv
  | S_Triv
  | V_Triv
  | T_Pair
  | S_Pair
  | A_Pair
  | A_Pair_GT
  | V_Pair
  | E_Pair
  | T_LetPair
  | S_LetPair
  | S_LetPair_GT
  | A_LetPair
  | A_LetPair_GT
  | E_LetPair
  | T_PrjL
  | S_PrjL
  | S_PrjL_GT
  | E_PrjL
  | T_PrjR
  | S_PrjR
  | S_PrjR_GT
  | E_PrjR => Product
  | T_InjL
  | A_InjL
  | A_InjL_GT
  | V_InjL
  | E_InjL
  | T_InjR
  | A_InjR
  | A_InjR_GT
  | V_InjR
  | E_InjR
  | T_Case
  | S_Case
  | S_Case_GT
  | A_Case
  | A_Case_GT
  | E_Case_L
  | E_Case_R => Sum
  | T_FixAnn
  | T_FixAnn_TV
  | T_Fix
  | E_Fix => Fixpoint
  | T_Roll
  | V_Roll
  | E_Roll
  | T_Unroll
  | E_Unroll => Recursive
  | A_Subsumption
  | A_Subsumption_GT => A_Subsumption
  | E_Val => E_Val
  | Assumption
  | And_I
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E => PropositionalLogic;

[@deriving (show({with_path: false}), sexp, yojson)]
type version =
  | Logic
  | AL
  | ALB
  | ALF
  | ALFp
  | ALFpBD
  | ALFA
  | ALFArec
  | ALFArecTV
  | GradualALFA;

let of_version: t => version =
  fun
  | Assumption
  | And_I
  | And_E_L
  | And_E_R
  | Or_I_L
  | Or_I_R
  | Or_E
  | Implies_I
  | Implies_E
  | Truth_I
  | Falsity_E => Logic
  | V_Num
  | E_Neg
  | E_Plus
  | E_Minus
  | E_Times => AL
  | V_True
  | V_False
  | E_Val
  | E_Lt_T
  | E_Lt_F
  | E_Gt_T
  | E_Gt_F
  | E_Eq_T
  | E_Eq_F
  | E_If_T
  | E_If_F => ALB
  | V_Fun
  | E_Let
  | E_Ap
  | T_Var
  | T_Let
  | T_LetAnn
  | T_Num
  | T_True
  | T_False
  | T_Neg
  | T_Plus
  | T_Minus
  | T_Times
  | T_Lt
  | T_Gt
  | T_Eq
  | T_If
  | T_Fun
  | T_FunAnn
  | T_Ap => ALF
  | E_Pair
  | V_Pair
  | T_Pair
  | E_PrjL
  | E_PrjR
  | T_PrjL
  | T_PrjR
  | E_LetPair
  | T_LetPair
  | V_Triv
  | T_Triv => ALFp
  | S_Num
  | S_True
  | S_False
  | A_Subsumption
  | S_Var
  | A_Fun
  | S_FunAnn
  | A_FunAnn
  | S_Ap
  | S_Neg
  | S_Plus
  | S_Minus
  | S_Times
  | S_Lt
  | S_Gt
  | S_Eq
  | S_LetAnn
  | A_LetAnn
  | S_Let
  | A_Let
  | S_Pair
  | A_Pair
  | S_LetPair
  | A_LetPair
  | S_PrjL
  | S_PrjR
  | S_Triv
  | A_If
  | S_If => ALFpBD
  | A_InjL
  | A_InjR
  | T_InjL
  | T_InjR
  | E_InjL
  | E_InjR
  | V_InjL
  | V_InjR
  | A_Case
  | S_Case
  | T_Case
  | E_Case_L
  | E_Case_R => ALFA
  | TV_Num
  | TV_Bool
  | TV_Unit
  | TV_Arrow
  | TV_Prod
  | TV_Sum
  | TV_Rec
  | TV_TVar
  | T_LetAnn_TV
  | T_FunAnn_TV
  | E_Fix
  | T_Fix
  | T_FixAnn
  | T_FixAnn_TV
  | T_Roll
  | V_Roll
  | T_Unroll
  | E_Roll
  | E_Unroll => ALFArec
  | C_Refl
  | C_UnkL
  | C_UnkR
  | C_Sum
  | C_Prod
  | C_Arrow
  | MS_Hole
  | MS_Sum
  | MP_Hole
  | MP_Prod
  | MA_Hole
  | MA_Arrow
  | S_Hole
  | A_Subsumption_GT
  | S_If_GT
  | S_PrjL_GT
  | S_PrjR_GT
  | A_Pair_GT
  | S_LetPair_GT
  | A_LetPair_GT
  | A_InjL_GT
  | A_InjR_GT
  | A_Case_GT
  | S_Case_GT
  | A_FunAnn_GT
  | A_Fun_GT
  | S_Ap_GT => GradualALFA;

let all: list(t) = [
  // Gradual Typing
  // - Type consistency
  C_Refl,
  C_UnkL,
  C_UnkR,
  C_Sum,
  C_Prod,
  C_Arrow,
  // - Type Matched
  MS_Hole,
  MS_Sum,
  MP_Hole,
  MP_Prod,
  MA_Hole,
  MA_Arrow,
  // - Gradual Typing Rules
  S_Hole,
  A_Subsumption_GT,
  S_If_GT,
  S_PrjL_GT,
  S_PrjR_GT,
  A_Pair_GT,
  S_LetPair_GT,
  A_LetPair_GT,
  A_InjL_GT,
  A_InjR_GT,
  A_Case_GT,
  S_Case_GT,
  A_FunAnn_GT,
  A_Fun_GT,
  S_Ap_GT,
  A_Subsumption,
  // Type Validity
  TV_Num,
  TV_Bool,
  TV_Unit,
  TV_Arrow,
  TV_Prod,
  TV_Sum,
  TV_Rec,
  TV_TVar,
  // Typing
  // - Booleans
  T_True,
  S_True,
  T_False,
  S_False,
  T_If,
  S_If,
  A_If,
  // - Numbers
  T_Num,
  S_Num,
  T_Neg,
  S_Neg,
  T_Plus,
  S_Plus,
  T_Minus,
  S_Minus,
  T_Times,
  S_Times,
  T_Lt,
  S_Lt,
  T_Gt,
  S_Gt,
  T_Eq,
  S_Eq,
  // - Variables
  T_Var,
  S_Var,
  T_LetAnn,
  T_LetAnn_TV,
  S_LetAnn,
  A_LetAnn,
  T_Let,
  S_Let,
  A_Let,
  // - Functions
  T_FunAnn,
  T_FunAnn_TV,
  S_FunAnn,
  A_FunAnn,
  T_Fun,
  A_Fun,
  T_Ap,
  S_Ap,
  // - Products
  T_Triv,
  S_Triv,
  T_Pair,
  S_Pair,
  A_Pair,
  T_LetPair,
  S_LetPair,
  A_LetPair,
  T_PrjL,
  S_PrjL,
  T_PrjR,
  S_PrjR,
  // - Sums
  T_InjL,
  A_InjL,
  T_InjR,
  A_InjR,
  T_Case,
  S_Case,
  A_Case,
  // - Fixpoints
  T_Fix,
  T_FixAnn,
  T_FixAnn_TV,
  // - Recursive
  T_Roll,
  T_Unroll,
  // Values
  V_Num,
  V_True,
  V_False,
  V_Fun,
  V_Pair,
  V_Triv,
  V_InjL,
  V_InjR,
  V_Roll,
  // Evaluation
  // - Value Evaluation
  E_Val,
  // - Booleans
  E_If_T,
  E_If_F,
  // - Numbers
  E_Neg,
  E_Plus,
  E_Minus,
  E_Times,
  E_Lt_T,
  E_Lt_F,
  E_Gt_T,
  E_Gt_F,
  E_Eq_T,
  E_Eq_F,
  // - Variables
  E_Let,
  E_Ap,
  // - Products
  E_Pair,
  E_PrjL,
  E_PrjR,
  E_LetPair,
  // - Sums
  E_InjL,
  E_InjR,
  E_Case_L,
  E_Case_R,
  // - Fixpoints
  E_Fix,
  // - Recursive
  E_Roll,
  E_Unroll,
  // Propositional logic
  Assumption,
  And_I,
  And_E_L,
  And_E_R,
  Or_I_L,
  Or_I_R,
  Or_E,
  Implies_I,
  Implies_E,
  Truth_I,
  Falsity_E,
];

let keywords: t => list(string) =
  rule =>
    (show(rule) |> String.split_on_char('_'))
    @ (
      switch (of_kind(rule)) {
      | TypeConsistency => ["Type", "Consistency", "consist", "tc", "cons"]
      | TypeMatched => ["Type", "Matched", "match", "tm", "mat"]
      | TypeValidity => ["Type", "Validity", "typ", "tv", "val"]
      | Synthesis => ["Synthesis", "syn"]
      | Analysis => ["Analysis", "ana"]
      | Typing => ["Type", "Typing", "typ"]
      | Values => ["Values", "val"]
      | Evaluation => ["Evaluation", "eval"]
      | PropositionalLogic(prop_logic_kind) =>
        ["Propositional", "Logic", "prop"]
        @ (
          switch (prop_logic_kind) {
          | Assumption => ["assump", "asm"]
          | Introduction => ["Introduction", "Intro"]
          | Elimination => ["Elimination", "Elim"]
          }
        )
      }
    );

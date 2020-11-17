%{
  let mk_seq operand =
    Seq.mk operand []

  let mk_binop l op r =
    let seq = Seq.seq_op_seq l op r in
    seq

  let mk_pat_binop l op r =
    let OpSeq.OpSeq(_, l) = l in
    let OpSeq.OpSeq(_, r) = r in
    mk_binop l op r

  let mk_pat_parenthesized e =
    let operand = UHPat.Parenthesized(e) in
    let seq = Seq.mk operand [] in
    UHPat.mk_OpSeq seq

  let mk_application e args =
    let e = mk_seq e in
    let rec mk_app e args =
      match args with
      | [] -> e
      | x::xs -> (
        let x = mk_seq x in
        let opseq = mk_app x xs in
        mk_binop e Operators_Exp.Space opseq
      )
    in
    mk_app e args
%}

%token LET
%token IN
%token <string> INT
%token <string> FLOAT
%token TRUE FALSE
%token PLUS MINUS
%token MULT DIV
%token FPLUS FMINUS
%token FMULT FDIV
%token COLON
%token COLONCOLON
%token SEMICOLON
%token EQUAL
%token GREATER LESSER
%token PERIOD
%token COMMA
%token EOF
%token <string> IDENT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token LAMBDA
%token CASE
%token BAR
%token ARROW
%token TARROW
%token END
%token <string> COMMENT
%token EMPTY

%left LESSER GREATER EQUAL
%left PLUS MINUS FPLUS FMINUS
%left MULT DIV FMULT FDIV
%right COLONCOLON
%left BAR
%right TARROW
%left COMMA
%nonassoc LET LPAREN LAMBDA INT IDENT IN

%start main
%type <UHExp.t> main
%%

main:
  expr EOF { $1 }
  | expr SEMICOLON expr EOF { List.concat [$1; $3] }
;

let_binding:
  LET pat EQUAL expr IN { UHExp.letline $2 $4 }
  | LET pat COLON typ EQUAL expr IN {
    let typ = UHTyp.mk_OpSeq $4 in
    UHExp.letline $2 ~ann:typ $6
  }
;

typ:
  atomic_type { $1 }
  | l = typ typ_op r = typ {
    mk_binop l $2 r
  }
  | LPAREN typ RPAREN {
    let opseq = UHTyp.mk_OpSeq $2 in
    Seq.mk (UHTyp.Parenthesized opseq) []
  }
  | LBRACK typ RBRACK {
    let opseq = UHTyp.mk_OpSeq $2 in
    Seq.mk (UHTyp.List opseq) []
  }
;

%inline typ_op:
  COMMA { Operators_Typ.Prod }
  | TARROW { Operators_Typ.Arrow }
  | BAR { Operators_Typ.Sum }
;

atomic_type:
  IDENT { mk_seq UHTyp.Int }
;

pat:
  LPAREN pat RPAREN { mk_pat_parenthesized $2 }
  | IDENT { UHPat.mk_OpSeq (mk_seq (UHPat.var $1)) }
  | LBRACK RBRACK {
    let seq = mk_seq (UHPat.listnil ()) in
    UHPat.mk_OpSeq seq
  }
  | pat COLONCOLON pat {
    let seq = mk_pat_binop $1 Operators_Pat.Cons $3 in
    UHPat.mk_OpSeq seq
  }
;

expr:
  expr_ { [UHExp.ExpLine (UHExp.mk_OpSeq $1)] }
  | COMMENT expr { List.concat [[UHExp.CommentLine $1]; $2] }
  | EMPTY expr { List.concat [[UHExp.EmptyLine]; $2] }
  | let_binding expr { List.concat [[$1]; $2] }
;

expr_:
  simple_expr { mk_seq $1 }
  | case { mk_seq $1 }
  | simple_expr simple_expr+ { mk_application $1 $2 }
  | expr_ op expr_ { mk_binop $1 $2 $3 }
  | expr_ COLONCOLON expr_ {
    mk_binop $1 Operators_Exp.Cons $3
  }
  | LBRACK RBRACK {
    mk_seq (UHExp.listnil ())
  }
;

simple_expr:
  LPAREN expr RPAREN { UHExp.Parenthesized($2) }
  | constant { $1 }
  | IDENT { UHExp.var $1 }
  | fn { $1 }
;

fn:
  LAMBDA pat PERIOD LBRACE expr RBRACE {
    UHExp.lam $2 $5
  }
  | LAMBDA pat COLON typ PERIOD LBRACE expr RBRACE {
    let typ = UHTyp.mk_OpSeq $4 in
    UHExp.lam $2 ~ann:typ $7
  }
;

case:
  CASE expr rule+ END { UHExp.case $2 $3 }
;

rule:
  BAR pat ARROW expr { UHExp.Rule($2, $4) }
;

%inline op:
  PLUS { Operators_Exp.Plus }
  | MINUS { Operators_Exp.Minus }
  | MULT { Operators_Exp.Times }
  | DIV { Operators_Exp.Divide }
  | FPLUS { Operators_Exp.FPlus }
  | FMINUS { Operators_Exp.FMinus }
  | FMULT { Operators_Exp.FTimes }
  | FDIV { Operators_Exp.FDivide }
  | GREATER { Operators_Exp.GreaterThan }
  | LESSER { Operators_Exp.LessThan }
  | EQUAL EQUAL { Operators_Exp.Equals }
;

constant:
  INT { UHExp.intlit $1 }
  | FLOAT { UHExp.floatlit $1 }
  | TRUE { UHExp.boollit true }
  | FALSE { UHExp.boollit false }
;

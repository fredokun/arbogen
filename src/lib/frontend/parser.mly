%token <string> UIDENT LIDENT
%token EOF

(* options tokens *)
%token SET
%token <int> NUMI
%token <float> NUMF

(* grammar tokens *)
%token SEQ
%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE Z

(* precedence rules *)
%left PLUS
%left TIMES

%start start
%type <Options.parameter list * ParseTree.t> start

%%

(* Grammar entry point *)

start:
  options = list(option)
  rules = nonempty_list(rule)
  EOF
  { options, rules }


(* Options ************************************************)

option:
  SET id = LIDENT value = value { Options.Param (id, value) }

value:
  | f = NUMF    { Options.Vfloat f }
  | n = NUMI    { Options.Vint n }
  | s = LIDENT  { Options.Vstring s }


(* Production rules ***************************************)

rule:
  name = UIDENT
  EQUAL
  expr = expr
  { name, expr }

expr:
  | LPAREN e = expr RPAREN          { e }
  | e1 = expr PLUS e2 = expr        { Grammar.Union (e1, e2) }
  | e1 = expr TIMES e2 = expr       { Grammar.Product (e1, e2) }
  | uid = UIDENT                    { Grammar.Reference uid }
  | SEQ LPAREN e = expr RPAREN      { Grammar.Seq e }
  | LWEIGHT w = NUMI RWEIGHT        { Grammar.Z w }
  | Z                               { Grammar.Z 1 }
  | ONE                             { Grammar.Z 0 }

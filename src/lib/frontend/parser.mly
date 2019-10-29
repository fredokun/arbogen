%{ open ParseTree %}

%token <string> UIDENT LIDENT
%token EOF

(* options tokens *)
%token SET
%token <int> NUMI
%token <float> NUMF

(* grammar tokens *)
%token SEQ
%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE Z

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
  components = separated_nonempty_list(PLUS, component)
  { name, components }

component:
  elements = separated_nonempty_list(TIMES, element)
  { elements }

element:
  | uid = UIDENT                     { Elem uid }
  | SEQ LPAREN uid = UIDENT RPAREN   { Seq uid }
  | LWEIGHT w = NUMI RWEIGHT         { Z w }
  | Z                                { Z 1 }
  | ONE                              { Z 0 }

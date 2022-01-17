%token <string> UIDENT LIDENT
%token EOF

/* options tokens */
%token SET
%token <int> NUMI
%token <float> NUMF

/* grammar tokens */
%token SEQ
%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE Z

/* precedence rules */
%left PLUS
%left TIMES

%start start
%type <Options.parameter list * ParseTree.t> start

%%

/* Grammar entry point */

start:
  option_list rule_list EOF { $1, $2 }


/* Options ********************************************** */

/* Possibly empty list */
option_list:
  | { [] }
  | arbogen_option option_list { $1 :: $2 }

arbogen_option:
  SET LIDENT value { Options.Param ($2, $3) }

value:
  | NUMF    { Options.Vfloat $1 }
  | NUMI    { Options.Vint $1 }
  | LIDENT  { Options.Vstring $1 }


/* Production rules ************************************* */

/* non-empty list */
rule_list:
  | rule           { [$1] }
  | rule rule_list { $1 :: $2 }

rule:
  UIDENT EQUAL expr { $1, $3 }

expr:
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Grammar.Union ($1, $3) }
  | expr TIMES expr         { Grammar.Product ($1, $3) }
  | UIDENT                  { Grammar.Reference $1 }
  | SEQ LPAREN expr RPAREN  { Grammar.Seq $3 }
  | LWEIGHT NUMI RWEIGHT    { Grammar.Z $2 }
  | Z                       { Grammar.Z 1 }
  | ONE                     { Grammar.Z 0 }

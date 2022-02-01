%token <string> UIDENT LIDENT
%token EOF

/* options tokens */
%token SET
%token <int> NUMI
%token <float> NUMF

/* grammar tokens */
%token SEQ
%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE Z

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
  SET LIDENT value { ($2, $3) }

value:
  | NUMF    { Options.Value.Float $1 }
  | NUMI    { Options.Value.Int $1 }
  | LIDENT  { Options.Value.String $1 }


/* Production rules ************************************* */

/* non-empty list */
rule_list:
  | rule           { [$1] }
  | rule rule_list { $1 :: $2 }

rule:
  UIDENT EQUAL expr { $1, $3 }

/* Expressions ****************************************** */

expr:
  | union           { Grammar.union $1 }
  | pof             { $1 }

union:
  | pof PLUS pof    { [$1; $3] }
  | pof PLUS union  { $1 :: $3 }

/* Product or Factor */
pof:
  | product { Grammar.product $1 }
  | factor  { $1 }

product:
  | factor TIMES factor     { [$1; $3] }
  | factor TIMES product    { $1 :: $3 }

factor:
  | LPAREN expr RPAREN      { $2 }
  | UIDENT                  { Grammar.Ref $1 }
  | SEQ LPAREN expr RPAREN  { Grammar.Seq $3 }
  | LWEIGHT NUMI RWEIGHT    { Grammar.Z $2 }
  | Z                       { Grammar.Z 1 }
  | ONE                     { Grammar.Z 0 }

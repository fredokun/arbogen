%{
open Parsing
open Lexing
open Hashtbl
open Ast
%}


%token <int> NUMI
%token <float> NUMF
%token <string> IDENT

%token SEQ
%token EPSILON 
%token Z
%token UNION
%token PROD
%token CARD
%token LPAR
%token RPAR
%token COMMA
%token LEQ
%token GEQ
%token EQ

%token SET

%token EOF

%start start
%type <Ast.ast> start

%%

/* parameter list * statement list */
start:
 | options statement_list EOF { $1,$2 }
 | statement_list EOF { [],$1 }
 | EOF { raise End_of_file }

/* parameter list */
options:
 | option options { $1 :: $2 }
 | option { [$1] }

/* parameter */
option:
 | SET IDENT NUMF { Param ($2, Vfloat $3) }
 | SET IDENT NUMI { Param ($2, Vint $3) }
 | SET IDENT IDENT { Param ($2, Vstring $3) }

/* statement list */
statement_list:
  | statement COMMA statement_list {$1 :: $3}
  | statement {[$1]}

/* statement */
statement:
  | IDENT EQ expression {($1, $3)}

/* expression list */
expression_list:
  | expression { [$1] }
  | expression COMMA expression_list { $1 :: $3 }

/* expression */
expression:
  | EPSILON { Ast.Epsilon }
  | Z { Ast.Z }
  | IDENT { Ast.Id $1 }
  | UNION LPAR expression_list RPAR { Ast.Union $3 }
  | PROD LPAR expression_list RPAR { Ast.Prod $3 }
  /* | SUBST LPAR expression_list RPAR { $$ = newExpression($3, SUBST, NONE, 0); } */
  | SEQ LPAR expression RPAR { Ast.Sequence $3 }
  | SEQ LPAR expression COMMA CARD LEQ NUMI RPAR { Ast.LeqSeq ($7, $3) }
  | SEQ LPAR expression COMMA NUMI GEQ CARD RPAR { Ast.LeqSeq ($5, $3) }
  | SEQ LPAR expression COMMA CARD EQ NUMI RPAR { Ast.EqSeq ($7, $3) }
  | SEQ LPAR expression COMMA NUMI EQ CARD RPAR { Ast.EqSeq ($5, $3) }
  | SEQ LPAR expression COMMA CARD GEQ NUMI RPAR { Ast.GeqSeq ($7, $3) }
  | SEQ LPAR expression COMMA NUMI LEQ CARD RPAR { Ast.GeqSeq ($5, $3) }

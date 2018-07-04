%{
open Parsing
open Lexing
open Hashtbl
open Ast

let add_option a b =
  match a,b with
  | Some n, Some n' -> Some (n+n')
  | a, None -> a
  | None, b -> b

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

start:
 | options statement_list EOF { $1,$2 }
 | statement_list EOF { [],$1 }
 | EOF { raise End_of_file }


options:
 | option options { $1 :: $2 }
 | option { [$1] }


option:
 | SET IDENT NUMF { Param ($2, Vfloat $3) }
 | SET IDENT NUMI { Param ($2, Vint $3) }
 | SET IDENT IDENT { Param ($2, Vstring $3) }

statement_list:
  | statement COMMA statement_list {$1 :: $3}
  | statement {[$1]}

statement:
  | IDENT EQ expression {($1, $3)}

simple_expression:
  | EPSILON { ((Some 0), [])  }
  | Z { ((Some 1), []) }
  | IDENT { (None, [Some (Ast.Elem $1)])  }
  | SEQ LPAR IDENT RPAR { (Some 0, [Some (Ast.Seq $3)]) }
  /* | SEQUENCE LPAR expression COMMA CARD LEQ NUMBER RPAR { $$ = newExpression($3, SEQUENCE, LESS, $7); } */
  /* | SEQUENCE LPAR expression COMMA NUMBER GEQ CARD RPAR { $$ = newExpression($3, SEQUENCE, LESS, $5); } */
  /* | SEQUENCE LPAR expression COMMA CARD EQ NUMBER RPAR { $$ = newExpression($3, SEQUENCE, EQUAL, $7); } */
  /* | SEQUENCE LPAR expression COMMA NUMBER EQ CARD RPAR { $$ = newExpression($3, SEQUENCE, EQUAL, $5); } */
  /* | SEQUENCE LPAR expression COMMA CARD GEQ NUMBER RPAR { $$ = newExpression($3, SEQUENCE, GREATER, $7); } */
  /* | SEQUENCE LPAR expression COMMA NUMBER LEQ CARD RPAR { $$ = newExpression($3, SEQUENCE, GREATER, $5); } */

prod:
  | PROD LPAR component RPAR { $3 }

component:
 | simple_expression COMMA component
     {
       let w = add_option (fst $1) (fst $3) in
       match (snd $1) with
       | [] -> (w, snd $3)
       | e -> (w, e @ (snd $3))
     }
 | simple_expression { $1 }


simple_expression_list:
  | simple_expression COMMA simple_expression_list { $1 :: $3 }
  | prod COMMA simple_expression_list { $1 :: $3 }
  | prod { [$1] }
  | simple_expression { [$1] }

expression:
  | UNION LPAR simple_expression_list RPAR { $3 }
  | simple_expression { [$1] }
  | prod { [$1] }

open Parsing
open Lexing
open Hashtbl

%token <int> NUM
%token <string> VIDENT

%token SEQ

%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE

%token EOF

%start grammar
%type <Grammar.grammar> grammar

%%

grammar:
 | rules EOF { $1 }
 | EOF { raise End_of_file }

rules:
 | rule rules { $1::$2 }
 | rule { [$1] }

rule:
 | VIDENT EQUAL components { ($1, $3) }

components:
 | component PLUS components { $1::$3 }
 | component { $1 }

component:
 | elem TIMES component

seq:
SEQ LPAREN VIDENT RPAREN { SEQ $3$ }

elem:
VIDENT { ELEM $1 }

weight:
LWEIGHT NUM RWEIGHT { $2$ }

one:
ONE { 1 }

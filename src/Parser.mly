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


%token <int> NUM
%token <string> IDENT

%token SEQ

%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE Z

%token EOF

%start start
%type <Ast.ast> start

%%

start:
 | rules EOF { $1 }
 | EOF { raise End_of_file }

rules:
 | rule rules { $1::$2 }
 | rule { [$1] }


/* string * (int option * elem list) list */
rule:
 | IDENT EQUAL components { ($1, $3) }


/* (int option * (elem option) list) list */
components:
 | component PLUS components { $1::$3 }
 | component { [$1] }


/* int option * ((elem option) list) */
component:
 | sub_component TIMES component
     {
       let w = add_option (fst $1) (fst $3) in
       match (snd $1) with
       | None -> (w, snd $3)
       | e -> (w, e::(snd $3))
     }
 | sub_component { (fst $1, [snd $1]) }


/* int option * elem option */
sub_component:
 | elem { (None, Some $1) }
 | seq { (Some 0, Some $1) }
 | weight { ($1, None) }
 | z { ($1, None) }
 | one { ($1, None) }

seq:
 | SEQ LPAREN IDENT RPAREN { Ast.Seq $3 }

elem:
 | IDENT { Ast.Elem $1 }

weight:
 | LWEIGHT NUM RWEIGHT { Some $2 }

z:
 | Z { Some 1 }

one:
 | ONE { Some 0 }

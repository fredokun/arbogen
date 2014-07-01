%{
open Parsing
open Lexing
open Hashtbl

let add_option a b =
  match a,b with
  | Some n, Some n' -> Some (n+n')
  | a, None -> a
  | None, b -> b

%}


%token <int> NUM
%token <string> VIDENT

%token SEQ

%token PLUS EQUAL TIMES LWEIGHT RWEIGHT LPAREN RPAREN ONE

%token EOF

%start start
%type <Ast_parsed.grammar> start

%%

start:
 | rules EOF { $1 }
 | EOF { raise End_of_file }

rules:
 | rule rules { $1::$2 }
 | rule { [$1] }


/* string * (int option * elem list) list */
rule:
 | VIDENT EQUAL components { ($1, $3) }


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
 | one { ($1, None) }

seq:
 |SEQ LPAREN VIDENT RPAREN { Ast_parsed.SEQ $3 }

elem:
 |VIDENT { Ast_parsed.ELEM $1 }

weight:
 |LWEIGHT NUM RWEIGHT { Some $2 }

one:
 |ONE { Some 0 }

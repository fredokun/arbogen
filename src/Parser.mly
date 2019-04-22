%{
  let opt_cons x xs = match x with
    | None -> xs
    | Some x -> x :: xs
%}

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
%type <Options.parameter list * Grammar.grammar> start

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

/* string * (int option * elem list) list */
rule:
  name = UIDENT
  EQUAL
  components = separated_nonempty_list(PLUS, component)
  { name, components }

/* int * (elem list) */
component:
  | sc = sub_component TIMES c = component
    {
      let w, opt_elem = sc in
      let w', comp = c in
      w + w', opt_cons opt_elem comp
    }
 | comp = sub_component { fst comp, opt_cons (snd comp) [] }


/* int * elem list */
sub_component:
  | uid = UIDENT                     { 0, Some (Grammar.Elem uid) }
  | SEQ LPAREN uid = UIDENT RPAREN   { 0, Some (Grammar.Seq uid) }
  | LWEIGHT w = NUMI RWEIGHT         { w, None }
  | Z                                { 1, None }
  | ONE                              { 0, None }

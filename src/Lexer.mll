{
open Parser
open Hashtbl
open Parsing

let grammar_keyword_table = Hashtbl.create 2
let parameter_keyword_table = Hashtbl.create 2

let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add grammar_keyword_table kwd tok)
    [
      ("SEQ", SEQ)
    ]

let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add parameter_keyword_table kwd tok)
    [
      ("set", SET)
    ]

}

let uident = ['A'-'Z']['-' '_' 'a'-'z''0'-'9''A'-'Z']*
let lident = ['a'-'z']['-' '_' 'a'-'z''0'-'9''A'-'Z']*
let num_int = ['0'-'9']*
let num_float = ['0'-'9']*'.'['0'-'9']['0'-'9']*
let lparen = '('
let rparen = ')'
let lweight = "<z^"
let rweight = '>'
let z = "<z>"
let one = "<1>"

let plus = '+'
let times = '*'
let equal = "::="

let space = [' ' '\t']*
let newline = ['\n' '\r']
let comment = '#' [^ '\n' '\r']*

rule token = parse
  | space {token lexbuf}
  | comment {token lexbuf}
  | newline {
    Lexing.new_line lexbuf;
    token lexbuf
  }
  | uident as s {
    try
      Hashtbl.find grammar_keyword_table s
    with Not_found ->
      UIDENT (s)
  }
  | lident as s {
    try
      Hashtbl.find parameter_keyword_table s
    with Not_found ->
      LIDENT (s)
  }
  | num_int as n {
    NUMI (int_of_string n)
  }
  | num_float as n {
    NUMF (float_of_string n)
  }
  | z { Z }
  | one { ONE }
  | lweight { LWEIGHT }
  | rweight { RWEIGHT }
  | plus { PLUS }
  | equal { EQUAL }
  | times { TIMES }
  | lparen { LPAREN }
  | rparen { RPAREN } 
  | eof { EOF }
  | _ { failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }
{
}

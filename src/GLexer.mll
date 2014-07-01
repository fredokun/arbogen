{
open GParserYacc
open Hashtbl
open Parsing

let keyword_table = Hashtbl.create 2

let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
      ("SEQ", SEQ)
    ]
}

let var_ident = ['A'-'Z']['-' '_' 'a'-'z''0'-'9']*
let kwd_ident = ['A'-'Z']*
let num = ['0'-'9']*
let lparen = '('
let rparen = ')'
let lweight = "<z^"
let rweight = '>'
let one = "<1>"

let plus = '+'
let times = '*'
let equal = '='

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
  | kwd_ident as s {
    try
      Hashtbl.find keyword_table s
    with Not_found ->
      failwith ("Unknown keyword : " ^ s)
  }
  | var_ident as s {
    VIDENT (s)
  }
  | num as n {
    NUM (int_of_string n)
  }
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

{
open Parser
open Hashtbl
open Parsing

let grammar_keyword_table = Hashtbl.create 8
let operator_keyword_table = Hashtbl.create 8
    

let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add grammar_keyword_table kwd tok)
    [
      ("Sequence", SEQ);
      ("Union", UNION);
      ("Prod", PROD);
      ("Z", Z);
      ("set", SET);
      ("card", CARD);
      ("Atom", ATOM)
    ]

let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add operator_keyword_table kwd tok)
    [
      ("(", LPAR);
      (")", RPAR);
      (",", COMMA);
      ("<=", LEQ);
      ("=<", LEQ);
      (">=", GEQ);
      ("=>", GEQ);
      ("=", EQ)
    ]

}

let ident = ['a'-'z' 'A'-'Z']['-' '_' 'a'-'z''0'-'9''A'-'Z']*
let num_int = ['0'-'9']*
let num_float = ['0'-'9']*'.'['0'-'9']['0'-'9']*

let space = [' ' '\t']*
let newline = ['\n' '\r']
let comment = "//" [^ '\n' '\r']*
let operators = ['(' ')' '=' ','] | "<=" | "=<" | ">=" | "=>"

              
rule token = parse
  | space {token lexbuf}
  | comment {token lexbuf}
  | newline {
    Lexing.new_line lexbuf;
    token lexbuf
  }
  | ident as s {
    try
      Hashtbl.find grammar_keyword_table s
    with Not_found ->
      IDENT (s)
  }
  | num_int as n {
    NUMI (int_of_string n)
  }
  | num_float as n {
    NUMF (float_of_string n)
  }
  | operators as s {
      try
        Hashtbl.find operator_keyword_table s
      with Not_found ->
        failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf)
    }
  | eof { EOF }
  | _ { failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }
{
}

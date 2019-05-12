{
  open Parser

  let lident_or_kw =
    let kw_table = Hashtbl.create 2 in
    List.iter (fun (kw, tok) -> Hashtbl.add kw_table kw tok) [
      "set", SET
    ];
    fun lid ->
      try Hashtbl.find kw_table lid
      with Not_found -> LIDENT lid

  let uident_or_kw =
    let kw_table = Hashtbl.create 2 in
    List.iter (fun (kw, tok) -> Hashtbl.add kw_table kw tok) [
      "SEQ", SEQ
    ];
    fun uid ->
      try Hashtbl.find kw_table uid
      with Not_found -> UIDENT uid

}

let uident = ['A'-'Z']['-' '_' 'a'-'z''0'-'9''A'-'Z']*
let lident = ['a'-'z']['-' '_' 'a'-'z''0'-'9''A'-'Z']*
let num_int = ['0'-'9']*
let num_float = ['0'-'9']*'.'['0'-'9']['0'-'9']*

let comment = "//" [^ '\n' '\r']*
let newline = ['\n' '\r']

rule token = parse
  | [' ' '\t']     {token lexbuf}
  | newline        {Lexing.new_line lexbuf; token lexbuf}
  | comment        {token lexbuf}
  | uident as s    {uident_or_kw s}
  | lident as s    {lident_or_kw s}
  | num_int as n   {NUMI (int_of_string n)}
  | num_float as n {NUMF (float_of_string n)}
  | "<z>"          { Z }
  | "<1>"          { ONE }
  | "<z^"          { LWEIGHT }
  | ">"            { RWEIGHT }
  | "+"            { PLUS }
  | "::="          { EQUAL }
  | "*"            { TIMES }
  | "("            { LPAREN }
  | ")"            { RPAREN }
  | "/*"           { comment lexbuf }
  | eof            { EOF }
  | _              { failwith ("Unknown symbol " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | "*/"     { token lexbuf }
  | newline  { Lexing.new_line lexbuf; comment lexbuf }
  | eof      { failwith "unterminated comment" }
  | _        { comment lexbuf }

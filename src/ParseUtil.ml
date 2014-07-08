let parse_from_channel chan =
  let lexbuf = Lexing.from_channel chan in
  let res = Parser.start Lexer.token lexbuf in
  close_in chan;
  res  

let parse_from_file filename = 
  let chan = open_in filename in
  parse_from_channel chan

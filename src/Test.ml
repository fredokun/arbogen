open Parser
open Lexer
open Ast
open Grammar

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let ast = Parser.start Lexer.token lexbuf in
      let grammar = Ast.grammar_of_ast ast in
      print_endline (string_of_grammar grammar);
      print_newline();
      flush stdout
    done
  with End_of_file ->
    failwith "Zut"

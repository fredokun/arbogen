open Parser
open Lexer
open Ast
open Grammar
open CombSys

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let ast = Parser.start Lexer.token lexbuf in
      let grammar = Ast.grammar_of_ast ast in
      let completed_grm = completion grammar in
      let combsys = combsys_of_grammar completed_grm in
      print_endline (string_of_grammar grammar);
      print_newline();
      print_endline (string_of_grammar completed_grm);
      print_newline();
      print_endline (fst @@ string_of_combsys combsys);
      print_newline();
      flush stdout
    done
  with End_of_file ->
    failwith "Zut"

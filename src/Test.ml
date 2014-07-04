open Parser
open Lexer
open Ast
open Grammar
open CombSys
open OracleSimple
open WeightedGrammar

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let ast = Parser.start Lexer.token lexbuf in
      let grammar = Ast.grammar_of_ast ast in
      let combsys = combsys_of_grammar grammar in
      let (zmin,zmax,y) = searchSingularity combsys 0. 0.9 0.01 0.01 0.8 in
      let wgrm = weighted_grm_of_grm grammar y in
      print_endline (string_of_grammar grammar);
      print_newline();
      print_endline (fst @@ string_of_combsys combsys);
      print_newline();
      print_endline (string_of_weighted_grammar wgrm);
      print_newline();
      flush stdout
    done
  with End_of_file ->
    failwith "Zut"

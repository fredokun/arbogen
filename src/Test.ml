open Parser
open Lexer
open Ast
open Grammar
open CombSys
open OracleSimple
open WeightedGrammar
open Gen
open Util
open Printf


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
      print_endline (string_of_weighted_grammar wgrm);
      (* print_endline (fst @@ string_of_combsys combsys); wont compile on my pc therefore was commented*)
      print_newline();
      
      Random.self_init () ;
      (* let counters = init_counter grammar StringMap.empty in
	 let (first_rule,_) = List.hd grammar in 
	 let new_counters = StringMap.add first_rule 1 counters in
	 let res = sim 0 new_counters wgrm 100 first_rule in
	 printf "Size of Tree simulated %d\n" res; 
      *)
      generator grammar true 0 100000 110000 0.01 0.1 0.01 0.1 1000 100 100 0.9;   

      print_newline();
      flush stdout;   
    done
  with End_of_file ->
    failwith "Zut"

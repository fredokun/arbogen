(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Grammar                                       *
 * -------                                               *
 * Internal representation of grammars                   *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util
open CombSys
open Tree

(* Grammar encoding *)

type component = int * string list ;; (* weight , sub-components *) 
     
type rule = string * component list

type grammar = rule list ;;


(* example of grammar  
let bintree = [ ("BinNode", [ (1,["Leaf"]) ; 
                              (0,["BinNode";"BinNode"]) ]) ];;
*)

(* grammar completion *)

let names_of_component (_,comps) =
	List.fold_left (fun names name -> StringSet.add name names) (StringSet.empty) comps ;;

let names_of_rule (_,comps) = 
	List.fold_left (fun names comp -> StringSet.union (names_of_component comp) names) (StringSet.empty) comps ;;

let names_of_grammar grm =
	List.fold_left (fun gnames rule -> StringSet.union (names_of_rule rule) gnames) (StringSet.empty) grm ;;

let rule_names_of_grammar grm =
	List.fold_left (fun rnames (rname,_) -> StringSet.add rname rnames) StringSet.empty grm ;;

let leafs_of_grammar grm = 
  let leafs = StringSet.diff (names_of_grammar grm) (rule_names_of_grammar grm)
  in
  StringSet.fold (fun leaf l -> leaf::l) leafs [] ;;
        
let completion grm =
  let leafs = leafs_of_grammar grm
  in
  grm @ (List.fold_left (fun lrules leaf -> (leaf,[])::lrules) [] leafs) ;;
  
(* printing *)

let string_of_component (weight,refs) =
  let rec strz w =
    if w=0 then ""
    else " * <z>" ^ (strz (w-1))
  in let rec strrefs = function
    | [] -> ""
    | [ref] -> ref
    | ref::refs -> ref ^ " * " ^ (strrefs refs)
     in
     (strrefs refs) ^ (strz weight) ;;

 let string_of_rule (rname,comps) =
	 let rec strcomps = function
		| [] -> ""
		| [comp] -> (string_of_component comp) ^ " ;"
		| comp::comps -> (string_of_component comp) ^ " + " ^ (strcomps comps)
	 in (rname ^ " ::= " ^ (strcomps comps)) ;;

let rec string_of_grammar = function
  | [] -> ""
  | rul::rules -> (string_of_rule rul) ^ "\n" ^ (string_of_grammar rules) ;;

(* print_endline (string_of_grammar bintree);; *)

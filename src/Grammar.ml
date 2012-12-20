(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Grammar                                       *
 * -------                                               *
 * Internal representation of grammars                   *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util

(* Grammar encoding *)

type component = int * elem list ;; (* weight , sub-components *)
     
type rule = string * component list ;;

type grammar = rule list ;;


(* example of grammar  
let bintree = [ (ELEM("BinNode"), [ (1,[ELEM("Leaf")]) ; 
                              (0,[ELEM("BinNode");ELEM("BinNode")]) ]) ];;
let (plane_tree:(Elem.t * (int * Elem.t list) list) list) = [ (ELEM("T"),[(0,[SEQ("T")]);(0,[ELEM("Leaf")])]);(SEQ("T"),[(0,[ELEM("T")])])]
*)

(* grammar completion *)

(* StringSet.iter (fun x -> print_endline (name_of_elem x)) (names_of_grammar plane_tree);; *)

let names_of_component (_,comps) =
	List.fold_left (fun names elt -> StringSet.add (name_of_elem elt) names) (StringSet.empty) comps ;;

let names_of_rule (_,comps) = 
	List.fold_left (fun names comp -> StringSet.union (names_of_component comp) names) (StringSet.empty) comps ;;

let names_of_grammar grm =
	List.fold_left (fun gnames rule -> StringSet.union (names_of_rule rule) gnames) (StringSet.empty) grm ;;

let rule_names_of_grammar grm =
	List.fold_left (fun rnames (rname,_) -> StringSet.add rname rnames) (StringSet.empty) grm ;;

let leafs_of_grammar grm = 
	let leafs = StringSet.diff (names_of_grammar grm) (rule_names_of_grammar grm)
	in
	StringSet.fold (fun leaf l -> leaf::l) leafs [] ;;
        
let completion grm =
	let leafs = leafs_of_grammar grm
	in
	grm @ (List.fold_left (fun lrules leaf -> (leaf,[(0,[])])::lrules) [] leafs) ;;

let count elt liste =
	let rec count_rec e l i =
		match l with
		|[] -> i
		|p::q -> if e=p then count_rec e q (i+1) else count_rec e q i in
	count_rec elt liste 0

(* printing *)

(*TODO à revoir *)
let string_of_component (weight,refs) =
  let rec strz w =
    if w=0 then ""
    else " * <z>" ^ (strz (w-1))
  in let rec strrefs = function
    | [] -> ""
    | [ref] -> name_of_elem ref
    | ref::refs -> (name_of_elem ref) ^ " * " ^ (strrefs refs)
     in
     (strrefs refs) ^ (strz weight) ;;

let string_of_rule (rname,comps) =
	 let rec strcomps = function
		| [] -> ""
		| [comp] -> (string_of_component comp) ^ " ;"
		| comp::comps -> (string_of_component comp) ^ " + " ^ (strcomps comps)
	 in ((name_of_elem rname) ^ " ::= " ^ (strcomps comps)) ;;

let rec string_of_grammar = function
  | [] -> ""
  | rul::rules -> (string_of_rule rul) ^ "\n" ^ (string_of_grammar rules) ;;

(* print_endline (string_of_grammar bintree);; *)

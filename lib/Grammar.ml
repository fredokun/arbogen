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
  List.fold_right (fun name names -> StringSet.add names name) (StringSet.empty) comps ;;

let names_of_rule (_,comps) = 
  List.fold_right (fun comp names -> StringSet.union (names_of_component comp) names) (StringSet.empty) comps ;;

let names_of_grammar grm =
  List.fold_right (fun rule gnames -> StringSet.union (names_of_rule rule) gnames) (StringSet.empty) grm ;;

let rule_names_of_grammar grm =
  List.fold_right (fun (rname,_) rnames -> StringSet.add rnames rname) StringSet.empty gram ;;

let leafs_of_grammar grm = 
  let leafs = StringSet.diff (names_of_grammar grm) (rule_names_of_grammar grm)
  in
  StringSet.fold (fun leaf l -> leaf::l) leafs [] ;;
        
let completion grm =
  let leafs = leafs_of_grammar grm
  in
  grm @ (StringSet.fold (fun leaf lrules -> (leaf,[])::lrules) leafs [] leafs) ;;
  
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

let rec string_of_grammar = function
  | [] -> ""
  | rul::rules -> (string_of_rule rul) ^ "\n" ^ (string_of_grammar rules) ;;

let string_of_rule (rname,comps) =
  let rec strcomps = function
    | [] -> ""
    | [comp] -> (string_of_component comp) ^ " ;"
    | comp::comps -> (string_of_component comp) ^ " + " ^ (strcomps comps)
  in (rname ^ " ::= " ^ (strcomps comps)) ;;

(* print_endline (string_of_grammar bintree);; *)





(** a grammar rule is either a terminal (leaf) node or a non-terminal node *)
type rule = Terminal | NonTerminal of (string list list)

let n_rule (r:rule):int = match r with
	Terminal -> 0	
	|NonTerminal ll -> (List.length ll)

(** a grammar is a list of named, indexed rules *)
type grammar = (string * int * rule) list

let grastr (g:grammar) (i:int) : string = 
	let (p1,_,_) = List.nth g i in
	p1

let graint (g:grammar) (i:int) : int = 
	let (_,p2,_) = List.nth g i in
	p2

let grarule (g:grammar) (i:int) : rule = 
	let (_,_,p3) = List.nth g i in
	p3

let gra_findWeight (g:grammar) (r:string) : int = 
	let predica ((st,_,_):string*int*rule):bool =
		if String.compare r st = 0 then
			true
		else
			false
	in
	let rl = List.filter predica g in
	let (_,p2,_) = List.hd rl in
	p2

let rec tree_size (thetree:tree) (g:grammar) : int = 
	match thetree with
		Leaf thest ->
			let weileaf = gra_findWeight g thest in
			weileaf
		|Node(thest,thetl) ->
			let weinode = gra_findWeight g thest in
			let res = ref weinode in
			let l = List.length thetl in
			let theend = l - 1 in
			for i = 0 to theend do
				res := !res + (tree_size (List.nth thetl i) g)
			done;
		!res

	

let gra_findrule (g:grammar) (r:string) : rule = 
	let predica ((st,_,_):string*int*rule):bool =
		if String.compare r st = 0 then
			true
		else
			false
	in
	let rl = List.filter predica g in
	let (_,_,p3) = List.hd rl in
	p3



let gra_findTer (g:grammar) : string array = 
	let predica ((_,_,ru):string*int*rule):bool =
		match ru with
			Terminal -> true
			| NonTerminal _ -> false
	in 
	let rl = List.filter predica g in
	let lrl = List.length rl in
	let r = Array.make lrl "name" in
	let theend = lrl - 1 in
	for i = 0 to theend do
		let (p1,_,_) = List.nth rl i in
		r.(i) <- p1
	done;
	r;


		
	
	

	 
	
		


					
				
				
						

	

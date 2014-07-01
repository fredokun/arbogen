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

(* Elements of grammar *)
type elem = Seq of string | Elem of string
type reference = string

type component =  Call of reference | Cons of int * elem list

type rule = string * component list

type grammar = rule list


(* grammar completion *)

let name_of_elem (elt:elem) =
	match elt with
		|Seq(name) -> name
		|Elem(name) -> name ;;

let names_of_component comp =
  let names = StringSet.empty in
  match comp with
  | Call ref -> StringSet.add ref names
  | Cons (_, l) -> List.fold_left (fun names elt -> StringSet.add (name_of_elem elt) names) names l


let names_of_rule (_,comps) = 
	List.fold_left (fun names comp -> StringSet.union (names_of_component comp) names) (StringSet.empty) comps

let names_of_grammar (grm:grammar) =
	List.fold_left (fun gnames rule -> StringSet.union (names_of_rule rule) gnames) (StringSet.empty) grm

let rule_names_of_grammar (grm:grammar) =
	List.fold_left (fun rnames (rname,_) -> StringSet.add rname rnames) (StringSet.empty) grm

let leafs_of_grammar (grm:grammar) = 
	let leafs = StringSet.diff (names_of_grammar grm) (rule_names_of_grammar grm)
	in
	StringSet.fold (fun leaf l -> leaf::l) leafs []
        
let completion (grm:grammar) =
	let leafs = leafs_of_grammar grm
	in
	grm @ (List.fold_left (fun lrules leaf -> (leaf,[Cons (0,[])])::lrules) [] leafs)

(* printing *)

let string_of_elem = function
  | Elem name -> name
  | Seq name -> "Seq(" ^ name ^ ")"

let string_of_component comp =
  let strz w =
    if w=0 then ""
    else " * <z^" ^ (string_of_int w) ^ ">"
  in
  let rec strcons cons_list=
    match cons_list with
    | [] -> ""
    | [ref] -> string_of_elem ref
    | ref::refs -> (string_of_elem ref) ^ " * " ^ (strcons refs)
  in
  match comp with
  | Call e -> "Call(" ^ e ^ ")"
  | Cons (weight, cons_list) -> "Cons(" ^ (strcons cons_list) ^ (strz weight) ^ ")"

let string_of_rule (rname,comps) =
	 let rec strcomps = function
		| [] -> ""
		| [comp] -> (string_of_component comp) ^ " ;"
		| comp::comps -> (string_of_component comp) ^ " + " ^ (strcomps comps)
	 in let rstr = match comps with
	   | [] -> "<empty>"
	   | _ -> strcomps comps
	    in rname ^ " ::= " ^ rstr ;;

let rec string_of_grammar = function
  | [] -> ""
  | rul::rules -> (string_of_rule rul) ^ "\n" ^ (string_of_grammar rules)


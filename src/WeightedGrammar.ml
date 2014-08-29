(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: WeightedGrammar                               *
 * -------                                               *
 * Internal representation of Weighted grammars          *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *	     under the                                       *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util
open Grammar

type weighted_grammar = ( float * (Grammar.component * float) list ) StringMap.t

let rule_names_to_index grm =
  let rec rn_to_ind grm index_map index =
    match grm with
    | [] -> index_map
    | (rule_name, _) :: grm' ->
	    rn_to_ind grm' (StringMap.add rule_name index index_map) (index+1)
  in
  let index_map = StringMap.empty in
  rn_to_ind grm index_map 0

let cpnt_to_wcpnt z rules_indexes values component =
  match component with
  | Call ref as cpnt->
    begin
	    let rule_index = StringMap.find ref rules_indexes in
	    (cpnt, values.(rule_index))
    end
  | Cons (zn,l) as cpnt ->
    begin
	    let w = (z ** (float_of_int zn)) *.
        (List.fold_left
           (fun total_weight elem ->
            match elem with
		        | Elem name ->
		          begin
                let rule_index = StringMap.find name rules_indexes in
                total_weight *. values.(rule_index)
		          end
		        | Seq name ->
		          begin
                let rule_index = StringMap.find name rules_indexes in
                total_weight /. (1. -. values.(rule_index))
		          end
           )
           1.
           l)
	    in (cpnt, w)
    end

let weighted_grm_of_grm
    (grm:Grammar.grammar)
    (values:float array)
    (z:float)
    : weighted_grammar =
  let rec wgrm_of_grm
      (grm:Grammar.grammar)
      (wgrm:weighted_grammar)
      (rules_indexes:int StringMap.t)
      (values:float array)
      (z:float)
      : weighted_grammar =
    match grm with
    | [] -> wgrm
    | (rule_name, components) :: grm' ->
	    begin
        let components_weight = List.map (cpnt_to_wcpnt z rules_indexes values) components in
        let rule_weight = List.fold_left (fun r (_,w) -> r+.w) 0. components_weight in
        let wgrm' = StringMap.add rule_name (rule_weight, components_weight) wgrm in
        wgrm_of_grm grm' wgrm' rules_indexes values z
	    end
  in
  let rules_indexes = rule_names_to_index grm in
  let wgrm = StringMap.empty in
  wgrm_of_grm grm wgrm rules_indexes values z

let string_of_weighted_component comp =
  let strz w =
    if w=0 then ""
    else "<z^" ^ (string_of_int w) ^ ">"
  in
  let rec strcons cons_list=
    match cons_list with
    | [] -> "1"
    | [ref] -> string_of_elem ref
    | ref::refs -> (string_of_elem ref) ^ " * " ^ (strcons refs)
  in
  match comp with
  | (Call e), w  -> (string_of_float w) ^", Call(" ^ e ^ ")"
  | (Cons (weight, cons_list)), w ->
    if weight != 0 then
	    (string_of_float w) ^ ", Cons(" ^ (strcons cons_list) ^ " * " ^ (strz weight) ^ ")"
    else
	    (string_of_float w) ^ ", Cons(" ^ (strcons cons_list) ^  ")"

let string_of_weighted_rule (rname,weight,comps) =
  let rec strcomps = function
    | [] -> ""
    | [comp] -> (string_of_weighted_component comp) ^ " ;"
    | comp::comps -> (string_of_weighted_component comp) ^ " + " ^ (strcomps comps)
  in let rstr = match comps with
  | [] -> "<empty>"
  | _ -> strcomps comps
     in rname ^ ", " ^ (string_of_float weight)  ^ " ::= " ^ rstr ;;

let rec string_of_weighted_grammar wgrm =
  StringMap.fold
    (fun name (weight, components) s ->
      (string_of_weighted_rule (name,weight,components)) ^ "\n" ^ s)
    wgrm
    ""

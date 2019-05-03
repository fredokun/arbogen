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
  let zn, l = component in
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
  in (component, w)

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

let pp_component fmt (component, weight) =
  Format.fprintf fmt "%F, %a" weight Grammar.pp_component component

let pp_components fmt = function
  | [] -> Format.fprintf fmt "<empty>"
  | components ->
    let pp_sep fmt () = Format.fprintf fmt " + " in
    Format.pp_print_list ~pp_sep pp_component fmt components

let pp fmt wgrm =
  StringMap.iter
    (fun name (weight, components) ->
       Format.fprintf fmt "%s, %F ::= %a" name weight pp_components components)
    wgrm

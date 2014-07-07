(********************************************************
* Arbogen-lib : fast uniform random generation of trees *
*********************************************************
* Module: Gen                                           *
* -------                                               *
* The Boltzmann random generator                        *
* -------                                               *
* (C) 2011, Xuming Zhan, Frederic Peschanski            *
*           Antonine Genitrini, Matthieu Dien           *
*           under the                                   *
*           GNU GPL v.3 licence (cf. LICENSE file)      *
*********************************************************)

open Printf

open Options
open Tree
open Util
open CombSys
open WeightedGrammar
open OracleSimple
open Grammar



let rec find_component rdm_float componentList = 
  match componentList with
    | [comp] -> comp
    | comp::list_comp -> let (composant,freq) = comp in
			 if rdm_float <= freq then
                           comp
			 else
                           find_component (rdm_float-.freq) list_comp
    | _ -> failwith "find_component failed !!!" 

let rec get_next_rule (name_rule:string) (wgrm:WeightedGrammar.weighted_grammar) =      
  let (total_weight,component_list) = (StringMap.find name_rule wgrm) in
  let rdm_float = Random.float total_weight in
  let comp = (find_component rdm_float component_list) in
  match comp with
    | (Grammar.Call elem), _ -> get_next_rule elem wgrm 
    | (Grammar.Cons( w, elem_list)), _ ->
      (w, 
      List.fold_left 
	(fun next_rules elem ->
          match elem with
            | (Grammar.Elem name) -> name :: next_rules
            | (Grammar.Seq name) -> let (w,_) = StringMap.find name_rule wgrm in
                                    let n' = int_of_float (floor((log( Random.float 1.)) /. (log w))) in
                                    next_rules @ (concat_n [name] (n'-1))
	)
	[]
	elem_list)


let rec count_rules counters elements =
  match elements with 
    |elem::elems -> let nb = StringMap.find elem counters in
                    let new_map = StringMap.add elem (nb+1) counters in
                    count_rules new_map elems;
    | _ -> counters

let  find_non_zero counters = 
  let filterd_map = StringMap.filter (fun _ n -> -n <> 0) counters in
  fst (StringMap.choose filterd_map)

let rec sim(size:int) counters (wgrm:WeightedGrammar.weighted_grammar) (sizemax:int) (current_rule:string) =
  if (StringMap.for_all (fun _ n -> n = 0 ) counters) || (size>sizemax)  then
    size
  else
    let(_,_) = (StringMap.find current_rule wgrm) in 
    let (total_weight,next_rules) = get_next_rule current_rule wgrm in
    if (List.length next_rules) > 0 then
      let new_counters = (count_rules counters (List.tl next_rules)) in  
      sim (size+total_weight) new_counters wgrm sizemax  (List.hd next_rules)
    else
      let non_zero = find_non_zero counters in
      let nb = StringMap.find non_zero counters in
      let new_nb = nb - 1 in
      sim (size+total_weight) (StringMap.add non_zero new_nb counters) wgrm sizemax non_zero
        
let rec init_counter g map =
  match g with
    | rul::rules -> StringMap.add (fst rul) 0 map
    | _ -> map 
 


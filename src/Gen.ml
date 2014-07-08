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

let rec init_counter g map =
  match g with
  | rul::rules -> StringMap.add (fst rul) 0 map
  | _ -> map 

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



(* the loop to generate nb_try trees with the same Seed *)
let rec gen_aux_aux wgrm grm nb_try nb_smaller nb_bigger sizemin sizemax  =
  if nb_try > 0 then
    let counters = init_counter grm StringMap.empty in
    let (first_rule,_) = List.hd grm in 
    let new_counters = StringMap.add first_rule 1 counters in
    let res = sim 0 new_counters wgrm sizemax first_rule in
    printf "res = %d\n" res;
    if res < sizemin then
      begin
        printf "smaller \n";
        gen_aux_aux wgrm grm (nb_try - 1)  (nb_smaller+1) nb_bigger sizemin sizemax
      end
    else if res > sizemax then
      begin
        printf "bigger \n";
        gen_aux_aux wgrm grm (nb_try - 1)  nb_smaller (nb_bigger+1) sizemin sizemax
      end
    else
      (Some(res),nb_smaller,nb_bigger)
    else
      (None,nb_smaller,nb_bigger)

(* the loop to change the seed each time and loop nb_change_seed times *)
let rec gen_aux wgrm grm nb_change_seed nb_try nb_smaller nb_bigger = 
  if nb_change_seed > 0 then
    let new_seed = Random.int 11231231 in
    Random.init new_seed;
    let (size,new_nb_smaller,new_nb_bigger) = gen_aux_aux wgrm grm nb_try 0 0 nb_smaller nb_bigger in
    match size with
    | Some size -> (Some(size),(new_nb_smaller+nb_smaller),(nb_bigger+new_nb_bigger)) 
    | None -> gen_aux wgrm grm (nb_change_seed - 1) nb_try (new_nb_smaller+nb_smaller) (nb_bigger+new_nb_bigger)
  else
    (None,nb_smaller,nb_bigger)

      (* the loop the refines the values after each time and loops nb_refine_seed times should also do the ratio thing for now not sure about it*)
let rec gen nb_refine_seed nb_change_seed nb_try g epsilon1 epsilon2 zmin zmax zstart epsilon1_factor epsilon2_factor sys sizemin sizemax= 
  printf "just called gen : nb : %d\n" nb_refine_seed;
  let (zmin',zmax',y) = 
    (if global_options.verbosity >= 2
     then printf "[ORACLE]: search singularity at z=%f\n%!" zstart) ;
    searchSingularity sys zmin zmax epsilon1 epsilon2 zstart in
  (if global_options.verbosity >= 2
   then printf "          ==> found singularity at z=%f\n%!" zmin');
  let wgrm = weighted_grm_of_grm g y in     
  let (size,nb_smaller,nb_bigger) = gen_aux wgrm g nb_change_seed nb_try sizemin sizemax in
  match size with
  | Some size -> Some(size)
  | None  -> if nb_refine_seed > 0 then
      gen  (nb_refine_seed - 1)  nb_change_seed nb_try g (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) zmin' zmax'  zstart epsilon1_factor epsilon2_factor sys sizemin sizemax
    else
      None 



(* max_try avec un seed ,  change du seed repeter max_try (max_refine_seed fois) si ca marche pas en refine max_refine_seed  *)        

let generator
    (g:grammar)
    (self_seed:bool)
    (seed:int)
    (sizemin:int)
    (sizemax:int)
    (epsilon1:float)
    (epsilon1_factor:float)
    (epsilon2:float)
    (epsilon2_factor:float)
    (max_try:int)
    (ratio_rejected:float)
    (max_refine:int)
    (max_refine_seed:int)
    (zstart:float)
    =
  (if self_seed
   then Random.self_init ()
   else Random.init seed) ;
  let sys = combsys_of_grammar (completion g) in
  (if global_options.verbosity >= 2
   then printf "[GEN]: combinatorial system is:\n%s\n%!" (fst (string_of_combsys sys))
  ); 
  let final_size = gen max_refine max_refine_seed max_try g epsilon1 epsilon2 0. 1. zstart epsilon1_factor epsilon2_factor sys sizemin sizemax in
  match final_size with 
  | Some final_size -> printf "J'ai trouve %d \n" final_size;
  | None -> printf "J'ai rien trouve";

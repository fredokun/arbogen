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
open Grammar
open OracleSimple
    


(* let pondere2 (g:grammar) (y:float array)
	(*: (float StringMap * (string list * float) list StringMap )*) =
  let g_comp = completion g in
  let ymap =
    List.fold_left2
      (fun map coef (name,_) -> StringMap.add name coef map)
      (StringMap.empty)
      (Array.to_list y)
      g_comp
  in
  (* calcule la pondération correspondante à un sous composants *)
  let aux2 = fun (_,componentList) ->
    let proba =
      List.fold_left
	(fun a b -> a *. (StringMap.find (name_of_elem b) ymap))
	1.
	componentList
    in
    let len = if (List.length componentList = 1) &&
	(List.exists (fun x -> x = (name_of_elem (List.hd componentList))) (leafs_of_grammar g)) then 0
      else List.length componentList
    in
    (componentList,len,proba)
  in
  (* renvoie la map des composants avec leurs sous composants (prochain fils) et pondération *)
  let aux = fun map (component,subComponents) ->
    let sub_component_list = List.map aux2 subComponents
    in
    let proba = (List.fold_left (fun a (_,_,f) -> f +. a) 0. sub_component_list)
    in
    StringMap.add
      component
      (sub_component_list,proba)
      map
  in
  List.fold_left aux StringMap.empty g_comp *)

let rec find_component rdm_float componentList = 
  match componentList with
  | [comp] -> comp
  | comp::list_comp -> let (freq,composant) = comp in
                            if rdm_float <= freq then
                              comp
                            else
                              find_component (rdm_float-.freq) list_comp
  | _ -> failwith "find_component failed !!!" 

let rec get_next_rule (name_rule:string) wgrm =      
  let (total_weight,component_list) = (StringMap.find name_rule wgrm) in
    let rdm_float = Random.float total_weight in 
      let comp = (find_component rdm_float component_list) in
        match comp with 
        | Call elem -> get_next_rule (name_of_elem elem) wgrm 

        | Cons w,elem_list -> List.fold_left 
                              (fun elem next_rules -> match elem with
                                                      | Elem name -> name :: next_rules
                                                      | Seq name -> let (w,_) = StringMap.find name_rule wgrm in
                                                                    let n' = int_of_float (floor((log( Random.float 1.)) /. (log w))) in
                                                                             next_rules @ (concat_n [name] n'-1)
                              )
                              []
                              elem_list 
        | _ -> printf "Lol"


let rec count_rules counters elements =
  match elements with 
  |elem::elems -> let nb = StringMap.find (name_of_elem elem) counters in
                          let new_map = StringMap.add (name_of_elem elem) (nb+1) counters in
                            count_rules new_map elems;
  | _ -> counters

let  find_non_zero counters = 
  let filterd_map = StringMap.filter (fun _ n -> n <> 0) counters in
    StringMap.choose filterd_map

let rec sim(size:int) counters wgrm (sizemax:int) leafs current_rule =
  if (StringMap.for_all (fun _ n -> n == 0 ) counters) || (size>sizemax)  then
    size
  else
      if (StringSet.exists (fun n -> (n == (fst current_rule))) leafs) then
         let(total_weight,_) = (StringMap.find (fst current_rule) wgrm) in
          sim (size+total_weight) counters wgrm sizemax leafs (find_non_zero counters)
      else
        let (poid,elem) = get_next_rule (fst current_rule) wgrm in
            let new_counters = (count_rules counters (List.tl next_rules)) in 
            sim (size+weight) new_counters wgrm sizemax leafs (List.hd elem)


 let generator
    (g:grammar)
    (self_seed:bool) (seed:int)
    (sizemin:int) (sizemax:int)
    (epsilon1:float) (epsilon1_factor:float)
    (epsilon2:float) (epsilon2_factor:float)
    (with_prefix:bool) (idprefix:string)
    (max_try:int) (ratio_rejected:float)
    (max_refine:int)(zstart:float)
    : (tree*int) option = 
    let sys = combsys_of_grammar(completion g) in
      let rec gen epsilon1 epsilon2 zmin zmax nb_refine zstart =
        let (zmin',zmax',y) = 
          (if global_options.verbosity >= 2
            then printf "[ORACLE]: search singularity at z=%f\n%!" zstart) ;
              searchSingularity sys zmin zmax epsilon1 epsilon2 zstart in
              (if global_options.verbosity >= 2
              then printf "          ==> found singularity at z=%f\n%!" zmin') ; 

        (* let rec try_gen(nb_try:int)(nb_smaller:int) : ((tree * int) option * int * int) =
         *)  
          Random.init 123123;
          let leafs = leafs_of_grammar g in 
          let(first_rule,_) = List.hd g in 
          let counters = StringMap.empty in
          (* need to fill the counter with 0 ?? *)
          let res = sim 0 counters wgrm sizemax (fst first_rule) in
            printf "res  : %d\n" res;
       in
          gen epsilon1 epsilon2 0. 1. 1 zstart 

        (* if self_seed
          then
            Random.init seed;
          else
            let rnd = Random.int 3142342 in
                Random.init rnd;

          let map = pondere2 g y in
          let leafs = leafs_of_grammar g in
          let (first_rule,_) = List.hd g in
          let counters = StringMap.empty in
              StringMap.add first_rule 1 counters; 
              let res = sim 0 counters map leafs sizemax in
              if res >= sizemin && res <= sizemax then
                  begin
                    Ranom.init seed;
                    gen_tree g with_prefix idprefix sizemax y;
                  end
                else
                  if self_seed then
                    failwith "Your trees are too big, change paramaters please"
                  else if nb_try > 0 then
                     if res < sizemin then
                        try_gen (nb_try-1) (nb_smaller+1) nb_bigger  
                    else
                        try_gen (nb_try-1) nb_smaller (nb_bigger+1)       (* Partie de rafinement on a le besoin ou pas ??? *)
          in
          gen epsilon1 epsilon2 0. 1. 1 zstart  *)

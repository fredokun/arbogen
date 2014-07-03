(*********************************************************
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


let getNext rule map =      (* from a rule i should be able to use the map to return the thing i need to apply*)




let rec sim(size:int)(mapQueue) map (sizemax:int) leafs = 
  if (StringMap.is_empty mapQueue) then                 (* if no longer have anything left then return size *)
    size
  else(
    if size<sizemax then(                        
        let next_rule = (List.hd mapQueue) in               (* get the next rule in mapQueue and decrease its value by one How to get the first element.*)
          size = size + 1;
        let nb = (StringMap.find next_rule mapQueue) in
            StringMap.remove next_rule mapQueue;
            if nb > 1 then
              StringMap.add next_rule (nb-1) mapQueue;
        let son = getNext next_rule map in                                                         
          if (List.exists son leafs) then(           (* dont add to the list but just add the poids to size till now assume poid is 1 *)
              size = size + 1;
          )else(
              if (List.exist mapQueue son) then
                let nb = StringMap.find son mapQueue in
                  StringMap.remove son mapQueue;
                  StringMap.add son (nb+1) mapQueue;
              else
                StringMap.add son 1;
              )
        sim size mapQueue map sizemax leafs;  
    )else
      size
)

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

        let rec try_gen(nb_try:int)(nb_smaller:int) : ((tree * int) option * int * int) =
          if self_seed
          then
            Random.init seed;
          else
            let rnd = Random.int 3142342 in
                Random.init rnd;

          let map = pondere2 g y in
          let leafs = leafs_of_grammar g in
          let (first_rule,_) = List.hd g in
          let mapQueue = StringMap.empty in
              StringMap.add first_rule 1 mapQueue; 
              let res = sim 0 mapQueue map leafs sizemax in
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
          gen epsilon1 epsilon2 0. 1. 1 zstart

(********************************************************
* Arbogen-lib : fast uniform random generation of trees *
*********************************************************
* Module: Gen                                           *
* -------                                               *
* The Boltzmann random generator                        *
* -------                                               *
* (C) 2011, Xuming Zhan, Frederic Peschanski            *
*           Antonine Genitrini, Matthieu Dien           *
*           Marwan Ghanem                               *
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
open GenState


let geom p =
  let rec g u s k l p =
    if u > s then
      let pk = p *. l in
      g u (s +. pk) (k + 1) l pk
    else
      k
  in
  let u = Random.float 1. in
  g u p 0 (1. -. p) p


let rec find_component (rdm_float:float) componentList =
  match componentList with
  | [comp] -> comp
  | comp::list_comp -> let (composant,freq) = comp in
			                 if rdm_float <= freq then
                         comp
			                 else
                         find_component (rdm_float-.freq) list_comp
  | _ -> failwith "find_component failed !!!"

let rec get_next_rule (name_rule:string) (wgrm:weighted_grammar) (isCall:bool) =
  let (total_weight,component_list) = (StringMap.find name_rule wgrm) in
  let rdm_float = (Random.float 1.) *. total_weight in
  let comp = (find_component rdm_float component_list) in
  match comp with
  | (Grammar.Call elem), _ -> get_next_rule elem wgrm true
  | (Grammar.Cons (w, elem_list)), _ ->
    begin
	    (w,
	     (List.fold_left
	        (fun next_rules elem ->
            match elem with
            | (Grammar.Elem name) -> name :: next_rules
            | (Grammar.Seq name) ->
              begin
                let (w,_) = StringMap.find name wgrm in
                (* let n' = geom w in *)
                let n' = int_of_float( snd (modf((log(Random.float 1.))/. (log(1.-.w))))) in
                (* printf "%d\n" n'; *)
                next_rules @ (concat_n [name] n')
              end
	        )
	        []
	        elem_list),
       isCall)
    end
	    
let rec init_counter (g:grammar) map =
  match g with
  | [] -> map
  | rul::rules -> init_counter rules (StringMap.add (fst rul) 0 map)

let rec count_rules counters elements =
  match elements with
  |elem::elems -> let nb = StringMap.find elem counters in
                  let new_map = StringMap.add elem (nb+1) counters in
                  count_rules new_map elems;
  | _ -> counters

let  find_non_zero counters =
  let filterd_map = StringMap.filter (fun _ n -> n <> 0) counters in
  if StringMap.is_empty filterd_map then
    None
  else
    Some(fst (StringMap.choose filterd_map))



let rec sim (size:int) counters (wgrm:WeightedGrammar.weighted_grammar) (sizemax:int) (current_rule:string) =
  if (size>sizemax)  then
    size
  else
    begin
      let (total_weight,next_rules,isCall) = get_next_rule current_rule wgrm false in
      if (List.length next_rules) > 0 then
	      begin
	        let new_counters = (count_rules counters (List.tl next_rules)) in
          sim (size+total_weight) new_counters wgrm sizemax  (List.hd next_rules)
	      end
      else
	      begin
	        let non_zero = find_non_zero counters in
	        match non_zero with
	        | Some s ->let nb = StringMap.find s counters in
                     let new_nb = nb - 1 in
                     sim (size+total_weight) (StringMap.add s new_nb counters) wgrm sizemax s
	        | None -> (size+total_weight)
	      end
    end




let rec simulate_seed (wgrm:WeightedGrammar.weighted_grammar)
    (grm:grammar) (nb_try:int) (nb_smaller:int) (nb_bigger:int) (sizemin:int) (sizemax:int)  =
  if nb_try > 0 then
    begin
      let counters = init_counter grm StringMap.empty in
      let (first_rule,_) = List.hd grm in
      let rdm_state = Random.get_state () in
      let res = sim 0 counters wgrm sizemax first_rule in
      if global_options.verbosity >= 3
      then printf "[SIM]: Simulated weight of tree = %d\n%!" res ;
      if res < sizemin then
        begin
          (if global_options.verbosity >= 3
           then printf "     ==> weight is too small => reject\n%!");
          simulate_seed wgrm grm (nb_try - 1)  (nb_smaller+1) nb_bigger sizemin sizemax
        end
      else if res > sizemax then
        begin
	        (if global_options.verbosity >= 3
           then printf "      ==> weight is too big\n%!") ;
          simulate_seed wgrm grm (nb_try - 1)  nb_smaller (nb_bigger+1) sizemin sizemax
        end
      else
        begin
	        (if global_options.verbosity >= 3
           then printf "     ==> simulated weight matches expected weight, select\n%!");
	        (Some(res),nb_smaller,nb_bigger,Some(rdm_state))
        end
    end
  else  (* max number of tries *)
    (None,nb_smaller,nb_bigger,None)



let rec simulator nb_refine nb_try g epsilon1 epsilon2 zmin zmax zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected=
  let (zmin',zmax',y) =
    (if global_options.verbosity >= 2
     then printf "[ORACLE]: search singularity at z=%f\n%!" zstart) ;
    searchSingularity sys zmin zmax epsilon1 epsilon2 zstart in
  (if global_options.verbosity >= 2
   then printf "          ==> found singularity at z=%f\n\n%!" zmin');
  let wgrm = weighted_grm_of_grm g y zmin' in
  (if global_options.verbosity >= 2
   then printf "[SIM]: weighted grammar is :\n%s\n%!" (WeightedGrammar.string_of_weighted_grammar wgrm));
  let (size,nb_smaller,nb_bigger,state) = simulate_seed wgrm g nb_try 0 0 sizemin sizemax in
  match size with
  | Some size ->
    (match state with
	  |Some state -> Some(size,state,wgrm)
	  |None -> assert false) (* unreachable case *)
  | None  ->
    if nb_refine > 0 then
      begin
        if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_bigger)) >= ratio_rejected then
	        simulator (nb_refine - 1)  nb_try g (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) zmin' zmax'  zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected
        else
	        failwith "try with other parameters Trees too big"
      end
    else
      None

let make_n_leaf_refs n =
  let rec aux n l =
    if n = 0 then
      l
    else
      aux (n-1) ((ref (Leaf("",""))) :: l)
  in
  aux n []

let rec gen_tree_rec counters stacks wgrm id current_rule with_prefix idprefix =
  let prefix = if with_prefix then idprefix ^ (string_of_int id) else (string_of_int id) in
  let (_,next_rules,_) = get_next_rule current_rule wgrm false in
  let arity = List.length next_rules in
  let refs = StringMap.find current_rule stacks in
  let current_ref = List.hd refs in
  let refs' = List.tl refs in
  let stacks' = StringMap.add current_rule refs' stacks in
  if arity = 0 then
    begin
      current_ref := Leaf(current_rule,prefix);
      match find_non_zero counters with
      | Some rule_name ->
        let counters' = StringMap.add rule_name ((StringMap.find rule_name counters) - 1) counters in
        gen_tree_rec counters' stacks' wgrm (id+1) rule_name with_prefix idprefix
      | None -> (id+1)
    end
  else
    begin
      let counters' = count_rules counters (List.tl next_rules) in
      let children_refs = make_n_leaf_refs arity in
      let stacks'' =
        List.fold_left2
          (fun stacks rule_name node_ref ->
            StringMap.add rule_name
              (node_ref :: (StringMap.find rule_name stacks))
              stacks)
          stacks'
          next_rules
          children_refs
      in
      current_ref := Node (current_rule, prefix, children_refs);
      gen_tree_rec counters' stacks'' wgrm (id+1) (List.hd next_rules) with_prefix idprefix
    end

let gen_tree (gen_state:gen_state) with_prefix idprefix =
  Random.set_state gen_state.rnd_state;
  let first_ref = ref (Leaf ("","")) in
  let wgrm = gen_state.weighted_grammar in
  let first_rule = gen_state.first_rule in
  let keys = StringMap.fold (fun k _ l -> k :: l) wgrm  [] in
  let counters = List.fold_left (fun map k -> StringMap.add k 0 map) StringMap.empty keys in
  let stacks =  List.fold_left
    (fun map k -> if k = first_rule then
        StringMap.add k [first_ref] map
      else
        StringMap.add k [] map)
    StringMap.empty keys in
  let size = gen_tree_rec counters stacks wgrm 0 first_rule with_prefix idprefix in
  (!first_ref, size)

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
    (with_prefix:bool)
    (idprefix:string)
    (max_try:int)
    (ratio_rejected:float)
    (max_refine:int)
    (zstart:float)
    =
  let seed2 =
    if self_seed then
      begin
	      Random.self_init ();
	      Random.int 1000000;
      end
    else
      seed
  in
  Random.init seed2;
  (if global_options.verbosity >= 2
   then printf "[GEN]: grammar parsed is :\n%s\n%!" (Grammar.string_of_grammar g)
  );
  printf "[SEED] starting seed = %d\n\n" seed2;
  let sys = combsys_of_grammar (completion g) in
  (if global_options.verbosity >= 2
   then printf "[GEN]: combinatorial system is:\n%s\n%!" (fst (string_of_combsys sys))
  );
  let res = simulator max_refine max_try g epsilon1 epsilon2 0. 1. zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected in
  match res with
  | Some(final_size,state,wgrm) ->
    let (first_rule,_) = List.hd g in
    let final_state = {rnd_state = state; weighted_grammar = wgrm; first_rule = first_rule} in
    let tree, size = gen_tree final_state with_prefix idprefix in
    Some(tree,size,final_state)				                	
  | None -> None


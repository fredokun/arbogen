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
  let rdm_float = Random.float total_weight in
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
           | (Grammar.Seq name) -> let (w,_) = StringMap.find name wgrm in
                                   let n' = int_of_float (floor((log( Random.float 1.)) /. (log w))) in
                                   next_rules @ (concat_n [name] n')
	       )
	       []
	       elem_list),
       isCall)
    end
	

let rec init_counter (g:grammar) map =
  match g with
  | [] -> map
  | rul::rules -> init_counter rules (StringMap.add (fst rul) 0 map)



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
    let counters = init_counter grm StringMap.empty in
    let (first_rule,_) = List.hd grm in
    let rdm_state = Random.get_state () in
    let res = sim 0 counters wgrm sizemax first_rule in
    if global_options.verbosity >= 3
    then printf "[SIM]: Generated tree of size = %d\n%!" res ;
    if res < sizemin then
      begin
        (if global_options.verbosity >= 3
         then printf "     ==> tree is too small => reject\n%!");
        simulate_seed wgrm grm (nb_try - 1)  (nb_smaller+1) nb_bigger sizemin sizemax
      end
    else if res > sizemax then
      begin
	      (if global_options.verbosity >= 3
         then printf "      ==> tree is too large\n%!") ;
        simulate_seed wgrm grm (nb_try - 1)  nb_smaller (nb_bigger+1) sizemin sizemax
      end
    else
      begin
	      (if global_options.verbosity >= 3
         then printf "     ==> tree matches expecte size, select\n%!");
	      (Some(res),nb_smaller,nb_bigger,Some(rdm_state))
      end
    else  (* max number of tries *)
      (None,nb_smaller,nb_bigger,None)



let rec simulator nb_refine_seed nb_try g epsilon1 epsilon2 zmin zmax zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected=
  let (zmin',zmax',y) =
    (if global_options.verbosity >= 2
     then printf "[ORACLE]: search singularity at z=%f\n%!" zstart) ;
    searchSingularity sys zmin zmax epsilon1 epsilon2 zstart in
  (if global_options.verbosity >= 2
   then printf "          ==> found singularity at z=%f\n\n%!" zmin');
  let wgrm = weighted_grm_of_grm g y in
  (if global_options.verbosity >= 2
   then printf "[SIM]: weighted grammar is :\n%s\n%!" (WeightedGrammar.string_of_weighted_grammar wgrm));
  let (size,nb_smaller,nb_bigger,state) = simulate_seed wgrm g nb_try 0 0 sizemin sizemax in
  match size with
  | Some size -> (match state  with
	  |Some state -> Some(size,state,wgrm)
	  |None -> failwith "should never be here")  (* unreachable case *)
  | None  -> if nb_refine_seed > 0 then
      begin
        if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_bigger)) >= ratio_rejected then
	        simulator (nb_refine_seed - 1)  nb_try g (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) zmin' zmax'  zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected
        else
	        failwith "try with other parameters Trees too big"
      end
    else
      None


type 'a queue = 'a Queue.t

let rec gen_stack_tree_rec (wgrm:WeightedGrammar.weighted_grammar) (size:int) (next_rules: string queue) rules =
  if (Queue.is_empty next_rules)  then
    (rules,size)
  else
    let current_rule = Queue.pop next_rules in
    let(total_weight,next_rules_list,_) = get_next_rule current_rule wgrm false in
    Stack.push (current_rule,(List.length next_rules_list)) rules;
    List.iter (fun elt -> Queue.push elt next_rules) next_rules_list;
    gen_stack_tree_rec wgrm (size+total_weight) next_rules rules

let gen_stack_tree (gen_state:gen_state) =
  Random.set_state gen_state.rnd_state;
  let queue = Queue.create () in
  Queue.push gen_state.first_rule queue;
  gen_stack_tree_rec gen_state.weighted_grammar 0 queue (Stack.create ())

let rec gen_tree_of_stack_rec
    (stack,size)
    (current_rules: tree queue)
    (with_prefix:bool) (idprefix:string) =
  match (Stack.is_empty stack) with
  |true -> ()
  |false -> let prefix = if with_prefix then idprefix ^ (string_of_int (size)) else (string_of_int (size)) in
	          let (rule,arity) = Stack.pop stack in
	          let next_rule =
		          if arity=0 then
		            Leaf(rule,prefix)
		          else
		            let sons = npop arity current_rules in Node(rule,prefix,sons)
	          in
	          Queue.push next_rule current_rules;
	          gen_tree_of_stack_rec (stack,size-1) current_rules with_prefix idprefix

let gen_tree_of_stack
    (stack,size)
    (with_prefix:bool) (idprefix:string) =
  let queue = Queue.create () in
  begin
    gen_tree_of_stack_rec (stack,size) queue with_prefix idprefix;
    ((Queue.pop queue),size)
  end

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
	      Random.int 11231231;
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
  let res = simulator max_refine  max_try g epsilon1 epsilon2 0. 1. zstart epsilon1_factor epsilon2_factor sys sizemin sizemax ratio_rejected in
  match res with
  | Some(final_size,state,wgrm) ->
    let (first_rule,_) = List.hd g in
    let final_state = {rnd_state = state; weighted_grammar = wgrm; first_rule = first_rule} in
    let (rules,res)  = gen_stack_tree final_state in
    let (tree,size) = gen_tree_of_stack (rules,res) with_prefix idprefix in
    Some(tree,size,final_state)				                	
  | None -> None


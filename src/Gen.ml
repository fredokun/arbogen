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

open Tree
open Util
open CombSys
open WeightedGrammar
open OracleSimple
open Grammar
open GenState
open RandGen

let rec find_component (rdm_float:float) componentList =
  match componentList with
  | [comp] -> comp
  | comp::list_comp -> let (_,freq) = comp in
    if rdm_float <= freq then
      comp
    else
      find_component (rdm_float-.freq) list_comp
  | _ -> failwith "find_component failed !!!"

let get_next_rule (name_rule:string) (wgrm:weighted_grammar) (isCall:bool) (name_called:string) (randgen: (module RandGen.Sig)) =
  let module Rand = (val randgen) in
  let (total_weight,component_list) = (StringMap.find name_rule wgrm.rules) in
  let rdm_float = (Rand.float 1.) *. total_weight in
  let comp = (find_component rdm_float component_list) in
  let (w, elem_list), _ = comp in
  (w,
   (List.fold_left
      (fun next_rules elem ->
         match elem with
         | (Grammar.Elem name) -> name :: next_rules
         | (Grammar.Seq name) ->
           begin
             let (w,_) = StringMap.find name wgrm.rules in
             let n' = int_of_float (snd (modf ((log (Rand.float 1.)) /. (log w)))) in
             next_rules @ (concat_n [name] n')
           end
      )
      []
      elem_list),
   isCall,name_called)

let init_counters wgrm = StringMap.map (fun _ -> 0) wgrm.rules

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



let rec sim (size:int) counters (wgrm:WeightedGrammar.weighted_grammar) (sizemax:int) (current_rule:string) (randgen: (module RandGen.Sig)) =
  if (size>sizemax)  then
    size
  else
    begin
      let (total_weight,next_rules,_,_) = get_next_rule current_rule wgrm false "" randgen in
      if (List.length next_rules) > 0 then
        begin
          let new_counters = (count_rules counters (List.tl next_rules)) in
          sim (size+total_weight) new_counters wgrm sizemax  (List.hd next_rules) randgen
        end
      else
        begin
          let non_zero = find_non_zero counters in
          match non_zero with
          | Some s ->let nb = StringMap.find s counters in
            let new_nb = nb - 1 in
            sim (size+total_weight) (StringMap.add s new_nb counters) wgrm sizemax s randgen
          | None -> (size+total_weight)
        end
    end

let sim_try
    (wgrm: WeightedGrammar.weighted_grammar)
    (nb_try: int)
    (sizemin: int)
    (sizemax: int)
    (randgen: (module RandGen.Sig))
    (verbosity: int) =
  let module Rand = (val randgen) in
  let counters0 = init_counters wgrm in

  let rec try_ nb_smaller nb_bigger = function
    | 0 -> None, nb_smaller, nb_bigger
    | nb_try ->
      let random_state = Rand.get_state () in
      let size = sim 0 counters0 wgrm sizemax wgrm.first_rule randgen in
      if verbosity >= 3 then Format.printf "[SIM]: Simulated size of tree = %d@." size;

      if size < sizemin then begin
        if verbosity >= 3 then Format.printf "     ==> weight is too small@.";
        try_ (nb_smaller + 1) nb_bigger (nb_try - 1)

      end else if size > sizemax then begin
        if verbosity >= 3 then Format.printf "      ==> weight is too big@.";
        try_ nb_smaller (nb_bigger + 1) (nb_try - 1)

      end else begin
        if verbosity >= 3 then Format.printf "     ==> simulated size is ok@.";
        Some (size, random_state), nb_smaller, nb_bigger
      end
  in
  try_ 0 0 nb_try

let compute_weighted_grammar grammar config verbosity =
  (* sanity check: I think the grammar should be complete at this point *)
  assert (completion grammar = grammar);
  let sys = combsys_of_grammar grammar in

  if verbosity >= 2 then Format.printf "[ORACLE]: search singularity at z=%F@." config.zstart ;
  let (zmin, zmax, values) = searchSingularity config sys in
  if verbosity >= 2 then Format.printf "          ==> found singularity at z=%F@." zmin;

  let wgrm = weighted_grm_of_grm grammar values zmin in
  if verbosity >= 2 then
    Format.printf "[SIM]: weighted grammar is :@\n%a@." WeightedGrammar.pp wgrm;
  (zmin, zmax, wgrm)

let rec simulator nb_refine nb_try g oracle_config epsilon1_factor epsilon2_factor sizemin sizemax ratio_rejected randgen verbosity =
  let (zmin, zmax, wgrm) = compute_weighted_grammar g oracle_config verbosity in
  let res, nb_smaller,nb_bigger = sim_try wgrm nb_try sizemin sizemax randgen verbosity in
  match res with
  | Some (size, state) -> Some (size, state, wgrm)
  | None when nb_refine > 0 ->
    if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_bigger)) >= ratio_rejected then
      let epsilon1 = oracle_config.epsilon1 *. epsilon1_factor in
      let epsilon2 = oracle_config.epsilon2 *. epsilon1_factor in
      let new_config = {epsilon1; epsilon2; zmin; zmax; zstart = zmin} in
      simulator (nb_refine - 1) nb_try g new_config epsilon1_factor epsilon2_factor sizemin sizemax ratio_rejected randgen verbosity
    else
      failwith "try with other parameters Trees too big"
  | None -> None

let make_n_leaf_refs n =
  let rec aux n l =
    if n = 0 then
      l
    else
      aux (n-1) ((ref (Leaf("",""))) :: l)
  in
  aux n []

let rec gen_tree_rec counters stacks wgrm id current_rule randgen =
  let prefix = string_of_int id in
  let (_,next_rules,_,name_called) = get_next_rule current_rule wgrm false current_rule randgen in
  let arity = List.length next_rules in
  let refs = StringMap.find current_rule stacks in
  let current_ref = List.hd refs in
  let refs' = List.tl refs in
  let stacks' = StringMap.add current_rule refs' stacks in
  if arity = 0 then
    begin
      current_ref := Leaf(name_called,prefix);
      match find_non_zero counters with
      | Some rule_name ->
        let counters' = StringMap.add rule_name ((StringMap.find rule_name counters) - 1) counters in
        gen_tree_rec counters' stacks' wgrm (id+1) rule_name randgen
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
      current_ref := Node (name_called, prefix, children_refs);
      gen_tree_rec counters' stacks'' wgrm (id+1) (List.hd next_rules) randgen
    end

let gen_tree (gen_state:gen_state) =
  let module Rand = (val (StringHashtbl.find randgen_tbl gen_state.randgen)) in
  Rand.set_state gen_state.rnd_state;
  let first_ref = ref (Leaf ("","")) in
  let wgrm = gen_state.weighted_grammar in
  let first_rule = gen_state.first_rule in
  let keys = StringMap.fold (fun k _ l -> k :: l) wgrm.rules [] in
  let counters = List.fold_left (fun map k -> StringMap.add k 0 map) StringMap.empty keys in
  let stacks =  List.fold_left
      (fun map k -> if k = first_rule then
          StringMap.add k [first_ref] map
        else
          StringMap.add k [] map)
      StringMap.empty keys in
  let size = gen_tree_rec counters stacks wgrm 0 first_rule (module Rand) in
  (!first_ref, size)

let init_rng ~randgen ~seed ~verbosity =
  let module Rand = (val StringHashtbl.find randgen_tbl randgen) in
  let seed = match seed with
    | Some seed -> seed
    | None -> Rand.self_init (); Rand.int 274537
  in
  if verbosity >= 2 then Format.printf "[SEED] starting seed = %d@." seed;
  Rand.init seed;
  (module Rand: RandGen.Sig)

let generator
    (g:grammar)
    ~seed:(seed: int option)
    (sizemin:int)
    (sizemax:int)
    (epsilon1:float)
    (epsilon1_factor:float)
    (epsilon2:float)
    (epsilon2_factor:float)
    (max_try:int)
    (ratio_rejected:float)
    (max_refine:int)
    (zstart:float)
    (randgen:string)
    (verbosity:int)
  =
  let randgen = init_rng ~randgen ~seed ~verbosity in
  let module Rand = (val randgen) in

  let oracle_config = OracleSimple.{epsilon1; epsilon2; zstart; zmin = 0.; zmax = 1.} in

  let res = simulator max_refine max_try g oracle_config epsilon1_factor epsilon2_factor sizemin sizemax ratio_rejected randgen verbosity in
  match res with
  | Some(size,state,wgrm) ->
    let (first_rule,_) = List.hd g in
    let final_state = {randgen = Rand.name; rnd_state = state; weighted_grammar = wgrm; first_rule = first_rule} in
    let tree, _ = gen_tree final_state in
    Some(tree,size,final_state)
  | None -> None

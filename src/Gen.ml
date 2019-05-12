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

open CombSys
open OracleSimple
open Grammar
open GenState
open RandGen

let select_component (module R: RandGen.Sig) rule =
  let open WeightedGrammar in
  let rec aux r = function
    | [] -> invalid_arg "select_component"
    | [{elems; atoms; _}] -> elems, atoms
    | {elems; atoms; weight} :: comps ->
      let r = r -. weight in
      if r < 0. then elems, atoms
      else aux r comps
  in
  match rule.choices with
  | [{elems; atoms; _}] -> elems, atoms
  | _ -> aux (R.float rule.weight) rule.choices

let gen_seq_len (module R: RandGen.Sig) rule x =
  let w = rule.WeightedGrammar.weight in
  let rec gen r wi =
    let r = r -. wi in
    if r < 0. then []
    else x :: gen r (wi *. w)
  in
  gen (R.float 1.) (1. -. w)

let sim (module R: RandGen.Sig) size_max rules =
  let rec gen_size s = function
    (* Generation complete *)
    | [] -> s

    (* Generate a tree of type [i]: draw a derivation among the possible
       derivations of [i] and add its components to the call stack *)
    | WeightedGrammar.Elem i :: next ->
      let elems, atoms = select_component (module R) rules.(i) in
      let s = s + atoms in
      if s > size_max then s
      else gen_size s (List.rev_append elems next)

    (* Generate a sequence of type [i]: draw the length of the list according
       to the geometric law and add the according number of [Elem i] to the
       call stack *)
    | WeightedGrammar.Seq i :: next ->
      let elems = gen_seq_len (module R) rules.(i) (WeightedGrammar.Elem i) in
      gen_size s (List.rev_append elems next)
  in
  gen_size 0 [Elem 0]

let sim_try wgrm (nb_try: int) (sizemin: int) (sizemax: int) (module R: RandGen.Sig) (verbosity: int) =
  let rec try_ nb_smaller nb_bigger = function
    | 0 -> None, nb_smaller, nb_bigger
    | nb_try ->
      let random_state = R.get_state () in
      let size = sim (module R) sizemax wgrm.WeightedGrammar.rules in
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

  let wgrm = WeightedGrammar.of_grammar zmin values grammar in
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

type instr =
  | Gen of WeightedGrammar.elem
  | Build of int

let rec build vals children = match vals with
  | None :: vals -> vals, children
  | Some tree :: vals -> build vals (tree :: children)
  | [] -> invalid_arg "build"

let gen (module R: RandGen.Sig) wg =
  let open WeightedGrammar in
  let {names; rules} = wg in
  let rec gen_tree size vals = function
    (* Generation complete *)
    | [] -> vals, size

    (* Build a node: pop [nb] trees form the generated stack and pack them as
       a single tree *)
    | Build rule_id :: next ->
      let vals, children = build vals [] in
      let tree = Tree.Node (names.(rule_id), children) in
      gen_tree size (Some tree :: vals) next

    (* Generate a tree of type [i]: select a derivation among the possible
       derivations of [i] and add its components to the call stack *)
    | Gen (Elem i) :: next ->
      let elems, atoms = select_component (module R) rules.(i) in
      let elems = List.map (fun e -> Gen e) elems in
      let next = List.rev_append elems (Build i :: next) in
      gen_tree (size + atoms) (None :: vals) next

    (* Generate a sequence of type [i]: draw the length of the list according
       to the geometric law and add the according number of [Gen Elem i] to
       the call stack *)
    | Gen (Seq i) :: next ->
      let elems = gen_seq_len (module R) rules.(i) (Gen (Elem i)) in
      gen_tree size vals (List.rev_append elems next)
  in
  match gen_tree 0 [] [Gen (Elem 0)] with
  | [Some tree], size -> tree, size
  | _ -> failwith "internal error"

let init_rng ~randgen ~seed ~verbosity =
  let module Rand = (val Util.StringHashtbl.find randgen_tbl randgen) in
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
    let final_state = {randgen = Rand.name; rnd_state = state; weighted_grammar = wgrm} in
    Rand.set_state state;
    let tree, size' = gen (module Rand) wgrm in
    assert (size = size');
    Some(tree,size,final_state)
  | None -> None

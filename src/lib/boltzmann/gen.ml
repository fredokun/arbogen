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

open GenState


(** {2 Core algorithms of the Boltzmann generation} *)

(** Select a rule component at random based on their weights *)
let select_component (module R: Randgen.Sig) rule =
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

(** Simulate a geometric law of parameter [w] *)
let gen_seq_len (module R: Randgen.Sig) w x =
  let rec gen r wi =
    let r = r -. wi in
    if r < 0. then []
    else x :: gen r (wi *. w)
  in
  gen (R.float 1.) (1. -. w)

(** Simulate the generation of a tree: only compute the size *)
let sim (module R: Randgen.Sig) size_max rules =
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
      let elems = gen_seq_len (module R) rules.(i).weight (WeightedGrammar.Elem i) in
      gen_size s (List.rev_append elems next)
  in
  gen_size 0 [Elem 0]

type instr =
  | Gen of WeightedGrammar.elem
  | Build of int

let rec build vals children = match vals with
  | None :: vals -> vals, children
  | Some tree :: vals -> build vals (tree :: children)
  | [] -> invalid_arg "build"

(** Generate a tree *)
let gen (module R: Randgen.Sig) wg =
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
      let elems = gen_seq_len (module R) rules.(i).weight (Gen (Elem i)) in
      gen_tree size vals (List.rev_append elems next)
  in
  match gen_tree 0 [] [Gen (Elem 0)] with
  | [Some tree], size -> tree, size
  | _ -> failwith "internal error"


(** {2 High level interface} *)

(** Search for a tree in a specific size window *)
let search_seed (type state) (module R: Randgen.Sig with type State.t = state) rules ~size_min ~size_max ~max_try
  : (int * state) option * int * int =
  let rec search rej_small rej_big nb_try =
    if nb_try = 0 then
      None, rej_small, rej_big
    else
      let state = R.get_state () in
      let nb_try = nb_try - 1 in
      let size = sim (module R) size_max rules in
      if size < size_min then
        search (rej_small + size) rej_big nb_try
      else if size > size_max then
        search rej_small (rej_big + size) nb_try
      else
        Some (size, state), rej_small, rej_big
  in
  search 0 0 max_try

(** Search for a size in a specific size window and refine the singularity
    search in case of failure *)
let rec simulator nb_refine max_try grammar oracle_config epsilon1_factor epsilon2_factor size_min size_max ratio_rejected randgen verbosity =
  let z, values = Oracles.Naive.make oracle_config grammar in
  let wgrm = WeightedGrammar.of_grammar ~z ~values grammar in
  let result, nb_smaller, nb_bigger = search_seed randgen wgrm.rules ~size_min ~size_max ~max_try in
  match result with
  | Some (size, state) -> Some (size, state, wgrm)
  | None when nb_refine > 0 ->
    if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_bigger)) >= ratio_rejected then
      let new_config = {oracle_config with
                        epsilon1 = oracle_config.epsilon1 *. epsilon1_factor;
                        epsilon2 = oracle_config.epsilon2 *. epsilon2_factor} in
      simulator (nb_refine - 1) max_try grammar new_config epsilon1_factor epsilon2_factor size_min size_max ratio_rejected randgen verbosity
    else
      failwith "try with other parameters Trees too big"
  | None -> None

let init_rng ~randgen ~seed ~verbosity =
  let module Rand = (val Randgen.get randgen) in
  let seed = match seed with
    | Some seed -> seed
    | None -> Rand.self_init (); Rand.int 274537
  in
  if verbosity >= 2 then Format.printf "[SEED] starting seed = %d@." seed;
  Rand.init seed;
  (module Rand: Randgen.Sig)

let generator
    grammar
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
  let oracle_config = Oracles.Naive.{epsilon1; epsilon2; zstart; zmin = 0.; zmax = 1.} in
  let module R = (val init_rng ~randgen ~seed ~verbosity) in
  let res = simulator
      max_refine
      max_try
      grammar
      oracle_config
      epsilon1_factor
      epsilon2_factor
      sizemin
      sizemax
      ratio_rejected
      (module R)
      verbosity
  in
  match res with
  | Some (size, state, wgrm) ->
    let final_state = {randgen = R.name; rnd_state = R.State.to_bytes state; weighted_grammar = wgrm} in
    R.set_state state;
    let tree, size' = gen (module R) wgrm in
    assert (size = size');
    Some (tree, size, final_state)
  | None -> None

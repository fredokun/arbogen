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


(** {2 Core algorithms of the Boltzmann generation} *)

(** Select a rule component at random based on their weights *)
let select_component (module R: Randtools.Sig.S) rule =
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

let gen_seq_len (module R: Randtools.Sig.S) p x =
  let n = Randtools.Distribution.geometric (module R) p in
  List.init n (fun _ -> x)

(** Simulate the generation of a tree: only compute the size *)
let free_size (module R: Randtools.Sig.S) size_max rules =
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
let free_gen (module R: Randtools.Sig.S) wg =
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
let search_seed
  (type state)
  (module R: Randtools.Sig.S with type State.t = state)
  rules
  ~size_min
  ~size_max
  ~max_try
  : (int * state) option =
  let rec search nb_try =
    if nb_try = 0 then None
    else
      let state = R.get_state () in
      let size = free_size (module R) size_max rules in
      if size < size_min || size > size_max then
        search (nb_try - 1)
      else
        Some (size, state)
  in
  search max_try

let generator grammar oracle rng ~size_min ~size_max ~max_try =
  let module R = (val rng: Randtools.Sig.S) in
  let wgrm = WeightedGrammar.of_grammar oracle grammar in
  match search_seed (module R) wgrm.rules ~size_min ~size_max ~max_try with
  | Some (size, state) ->
    R.set_state state;
    let tree, size' = free_gen rng wgrm in
    assert (size = size');  (* sanity check *)
    Some (tree, size)
  | None -> None

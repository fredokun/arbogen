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

(** {2 Free Bolzmann generators (low-level interface)} *)

let rec list_make_append n x l =
  if n <= 0 then l else list_make_append (n - 1) x (x :: l)

let free_size (module R : Randtools.S) ~size_max wgrm =
  let open WeightedGrammar in
  let rec gen_size s = function
    (* Generation complete *)
    | [] ->
      s
    (* Add the atom size to the total size and continue. *)
    | Z n :: next ->
      let s = s + n in
      if s > size_max then s else gen_size s next
    (* Lookup the definition of i and add it to the call stack *)
    | Ref i :: next ->
      gen_size s (wgrm.rules.(i) :: next)
    (* Draw the length of the list according to the geometric law and add the
       corresponding number of [expr] to the call stack *)
    | Seq (w, expr) :: next ->
      let n = Randtools.geometric (module R) w in
      gen_size s (list_make_append n expr next)
    (* Add all components of the product **in reverse order** to the call stack. *)
    | Product args :: next ->
      gen_size s (List.rev_append args next)
    (* Add one term of the union to the call stack and drop the other one. *)
    | Union args :: next ->
      gen_union_size s (R.float 1.) next args
  and gen_union_size s r next = function
    | [] ->
      assert false
    | (w, e) :: args ->
      if r <= w then gen_size s (e :: next)
      else gen_union_size s (r -. w) next args
  in
  gen_size 0 [wgrm.rules.(0)]

type instr =
  | Gen of WeightedGrammar.expression
  | Label of string
  | Tuple of int

let rec pop_n n acc xs =
  if n = 0 then (List.rev acc, xs)
  else
    match xs with
    | [] ->
      invalid_arg "pop_n"
    | x :: xs ->
      pop_n (n - 1) (x :: acc) xs

let pop = function [] -> invalid_arg "pop" | x :: xs -> (x, xs)

let relabel name : string Tree.t -> string Tree.t = function
  | Label _ as tree ->
    Label (name, [tree])
  | Tuple children ->
    Label (name, children)

let find_pos x xs =
  let len = Array.length xs in
  let rec aux i =
    if i = len then invalid_arg "find_pos"
    else if xs.(i) = x then i
    else aux (i + 1)
  in
  aux 0

let free_gen (module R : Randtools.S) wgrm =
  let open WeightedGrammar in
  let rec gen_tree size built = function
    (* Generation complete *)
    | [] ->
      (built, size)
    (* Build a named node. *)
    | Label name :: next ->
      let tree, built = pop built in
      gen_tree size (relabel name tree :: built) next
    (* Built a tuple *)
    | Tuple arity :: next ->
      let children, built = pop_n arity [] built in
      gen_tree size (Tree.Tuple children :: built) next
    (* Add the atom size to the total size and generate an empty tuple. *)
    | Gen (Z n) :: next ->
      gen_tree (size + n) (Tree.Tuple [] :: built) next
    (* Lookup the definition of i and add it to the call stack *)
    | Gen (Ref i) :: next ->
      gen_tree size built (Gen wgrm.rules.(i) :: Label wgrm.names.(i) :: next)
    (* Draw the length of the list according to the geometric law and add the
       corresponding number of [expr] to the call stack *)
    | Gen (Seq (w, expr)) :: next ->
      let n = Randtools.geometric (module R) w in
      gen_tree size built (list_make_append n (Gen expr) (Tuple n :: next))
    (* Add components of the product **in reverse order** to the call stack. *)
    | Gen (Product args) :: next ->
      gen_tree size built
        (List.fold_left
           (fun next e -> Gen e :: next)
           (Tuple (List.length args) :: next)
           args )
    (* Add one term of the union to the call stack and drop the other ones. *)
    | Gen (Union args) :: next ->
      gen_union size built (R.float 1.) next args
  and gen_union size vals r next = function
    | [] ->
      assert false
    | (w, e) :: args ->
      if r <= w then gen_tree size vals (Gen e :: next)
      else gen_union size vals (r -. w) next args
  in
  fun name ->
    let i = find_pos name wgrm.names in
    match gen_tree 0 [] [Gen (Ref i)] with
    | [tree], size ->
      (tree, size)
    | _ ->
      failwith "internal error"

(** {2 Rejection sampling in a size window (high-level interface)} *)

let search_seed (type state) (module R : Randtools.S with type State.t = state)
    ~size_min ~size_max ?max_try rules : (int * state) option =
  let rec search nb_try =
    if nb_try = 0 then None
    else
      let state = R.get_state () in
      let size = free_size (module R) ~size_max rules in
      if size < size_min || size > size_max then search (nb_try - 1)
      else Some (size, state)
  in
  search (Option.fold ~none:(-1) ~some:Fun.id max_try)

let generator grammar oracle rng ~size_min ~size_max ~max_try =
  let module R = (val rng : Randtools.S) in
  let wgrm = WeightedGrammar.of_grammar oracle grammar in
  match search_seed (module R) wgrm ~size_min ~size_max ~max_try with
  | Some (size, state) ->
    R.set_state state;
    let tree, size' = free_gen rng wgrm wgrm.names.(0) in
    assert (size = size');
    (* sanity check *)
    Some (tree, size)
  | None ->
    None

module WeightedGrammar = WeightedGrammar
module Oracle = Oracle

(** {2 Some shorthands} *)

let swap i j array =
  let xi = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- xi

let rec map_length f = function
  | [] -> [], 0
  | x :: xs ->
    let y = f x in
    let ys, len = map_length f xs in
    y :: ys, len + 1

let rec rev_append_n l1 l2 n = match n, l1 with
  | 0, _ -> l1, l2
  | _, x :: xs -> rev_append_n xs (x :: l2) (n - 1)
  | _ -> invalid_arg "rev_append_n"

(** {2 Persistent trees} *)

module Tree2 = struct
  type label = string
  type t = Node of label * t list

  let pp_label = Format.pp_print_string

  let rec pp ?(with_type=true) fmt tree =
    let Node (label, children) = tree in
    if with_type then pp_label fmt label;
    let pp_sep fmt () = Format.fprintf fmt "@\n" in
    Format.fprintf fmt "@[<1>[%a@]]" (Format.pp_print_list ~pp_sep pp) children
end

(** {2 A more efficient module for weighted grammars} *)

module WG = struct
  type elem = Elem of int | Seq of int
  type component = {weight: float; atoms: int; elems: elem list}
  type rule = {weight: float; choices: component list}
  type t = {rules: rule array; names: string array}

  let int_of_name names name =
    let exception Found of int in
    try
      Array.iteri (fun i n -> if n = name then raise (Found i)) names;
      invalid_arg "int_of_name"
    with Found i -> i

  let of_elem names = function
    | Grammar.Elem name -> Elem (int_of_name names name)
    | Grammar.Seq name -> Seq (int_of_name names name)

  let of_component names (comp, weight) =
    let atoms, gelems = comp in
    let elems = List.map (of_elem names) gelems in
    {weight; atoms; elems}

  let of_rule names (weight, components) =
    let choices = List.map (of_component names) components in
    {weight; choices}

  let of_wgrm wgrm =
    let module M = Util.StringMap in
    let open WeightedGrammar in
    let nb_rules = M.cardinal wgrm.rules in
    (* names generation *)
    let names = Array.make nb_rules "" in
    let _ = M.fold (fun name _ i -> names.(i) <- name; i + 1) wgrm.rules 0 in
    let i = int_of_name names wgrm.first_rule in
    swap i 0 names;
    (* rules generation *)
    let rules = Array.map (fun name -> of_rule names (M.find name wgrm.rules)) names in
    {rules; names}
end

let select_component (module R: RandGen.Sig) rule =
  let open WG in
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
  let w = rule.WG.weight in
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
    | WG.Elem i :: next ->
      let elems, atoms = select_component (module R) rules.(i) in
      let s = s + atoms in
      if s > size_max then s
      else gen_size s (List.rev_append elems next)

    (* Generate a sequence of type [i]: draw the length of the list according
       to the geometric law and add the according number of [Elem i] to the
       call stack *)
    | WG.Seq i :: next ->
      let elems = gen_seq_len (module R) rules.(i) (WG.Elem i) in
      gen_size s (List.rev_append elems next)
  in
  gen_size 0 [Elem 0]

type instr = Gen of WG.elem | Build of {rule: int; nb: int}

let gen (module R: RandGen.Sig) wg =
  let WG.{names; rules} = wg in
  let rec gen_tree vals = function
    (* Generation complete *)
    | [] -> vals

    (* Build a node: pop [nb] trees form the generated stack and pack them as
       a single tree *)
    | Build {rule; nb} :: next ->
      let vals, children = rev_append_n vals [] nb in
      let tree = Tree2.Node (names.(rule), children) in
      gen_tree (tree :: vals) next

    (* Generate a tree of type [i]: select a derivation among the possible
       derivations of [i] and add its components to the call stack *)
    | Gen (WG.Elem i) :: next ->
      let elems, _ = select_component (module R) rules.(i) in
      let elems, nb = map_length (fun e -> Gen e) elems in
      let next = List.rev_append elems (Build {rule = i; nb} :: next) in
      gen_tree vals next

    (* Generate a sequence of type [i]: draw the length of the list according
       to the geometric law and add the according number of [Gen Elem i] to
       the call stack *)
    | Gen (WG.Seq i) :: next ->
      let elems = gen_seq_len (module R) rules.(i) (Gen (WG.Elem i)) in
      gen_tree vals (List.rev_append elems next)
  in
  match gen_tree [] [Gen (WG.Elem 0)] with
  | [tree] -> tree
  | _ -> failwith "internal error"

(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: WeightedGrammar                               *
 * -------                                               *
 * Internal representation of Weighted grammars          *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *	     under the                                       *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

type elem = Elem of int | Seq of int
type component = {weight: float; atoms: int; elems: elem list}
type rule = {weight: float; choices: component list}
type t = {rules: rule array; names: string array}

module Smap = Map.Make(String)

let map_names_to_ids grammar =
  let names = List.map fst grammar |> Array.of_list in
  let _, indices = Array.fold_left
      (fun (i, map) name -> i + 1, Smap.add name i map)
      (0, Smap.empty)
      names
  in
  names, indices

(** {2 Conversion} *)

let eval_elem values = function
  | Elem i -> values.(i)
  | Seq i -> 1. /. (1. -. values.(i))

let of_elem indices = function
  | Grammar.Elem name -> Elem (Smap.find name indices)
  | Grammar.Seq name -> Seq (Smap.find name indices)

let of_component z values indices component =
  let atoms, elems = component in
  let elems = List.map (of_elem indices) elems in
  let atom_weight = z ** float_of_int atoms in
  let weight = Util.fold_map (eval_elem values) ( *. ) atom_weight elems in
  {weight; atoms; elems}

let of_rule z values indices rule =
  let _, components = rule in
  let choices = List.map (of_component z values indices) components in
  let weight = List.fold_left (fun w (comp: component) -> w +. comp.weight) 0. choices in
  {weight; choices}

let of_grammar z values grammar =
  let names, indices = map_names_to_ids grammar in
  let rules =
    List.map (of_rule z values indices) grammar |>
    Array.of_list
  in
  {names; rules}

(** {2 Pretty printing} *)

let pp_elem fmt = function
  | Elem i -> Format.fprintf fmt "Elem %d" i
  | Seq i -> Format.fprintf fmt "Seq %d" i

let pp_product pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " * " in
  match terms with
  | [] -> Format.fprintf fmt "1"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp_component fmt {weight; atoms; elems} =
  Format.fprintf fmt "(%F) z^%d * %a" weight atoms (pp_product pp_elem) elems

let pp_sum pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " + " in
  match terms with
  | [] -> Format.fprintf fmt "0"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp_rule fmt {weight; choices} =
  Format.fprintf fmt "(%F) %a" weight (pp_sum pp_component) choices

let pp fmt {rules; names} =
  Array.iteri
    (fun i rule ->
       Format.fprintf fmt "[%d] %s ::= %a@\n" i names.(i) pp_rule rule)
    rules

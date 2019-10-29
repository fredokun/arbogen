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

(** {2 Conversion from grammars} *)

let of_elem = function
  | Grammar.Elem i -> Elem i
  | Grammar.Seq i -> Seq i

let of_component oracle component =
  let weight = Oracles.Eval.component oracle component in
  let atoms, elems = component in
  let elems = List.map of_elem elems in
  {weight; atoms; elems}

let of_rule oracle rule =
  let weight = Oracles.Eval.rule oracle rule in
  let choices = List.map (of_component oracle) rule in
  {weight; choices}

let of_grammar oracle grammar =
  let Grammar.{names; rules} = grammar in
  let rules = Array.map (of_rule oracle) rules in
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

let pp_rule fmt rule =
  pp_sum pp_component fmt rule.choices

let pp fmt {rules; names} =
  Array.iteri
    (fun i rule ->
       Format.fprintf fmt "%s (%F) ::= %a@\n" names.(i) rule.weight pp_rule rule)
    rules

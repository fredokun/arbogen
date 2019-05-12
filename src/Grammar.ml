(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Grammar                                       *
 * -------                                               *
 * Internal representation of grammars                   *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

(* {2 Grammar encoding} *)

(** A grammar is a list of rules *)
type grammar = rule list

(** A rule is a non-terminal name and a list of production rules *)
and rule = string * component list

(** The product of an atom and a list of elements *)
and component = int * elem list

(** Either a non-terminal or a sequence (Kleene star) *)
and elem =
  | Elem of string
  | Seq of string

let epsilon = (0, [])

(** [make_epsilon_rule name] build a rule of the form [name ::= ε] *)
let make_epsilon_rule name =
  name, [(0, [])]


(** {2 Grammar completion} *)

module Sset = Set.Make(String)

let name_of_elem = function
  | Seq name -> name
  | Elem name -> name

let names_of_component (_, elems) =
  List.fold_left
    (fun names elem -> Sset.add (name_of_elem elem) names)
    Sset.empty
    elems

let names_of_rule (_, comps) =
  List.fold_left
    (fun names comp -> Sset.union (names_of_component comp) names)
    Sset.empty
    comps

let names_of_grammar grammar =
  List.fold_left
    (fun gnames rule -> Sset.union (names_of_rule rule) gnames)
    Sset.empty
    grammar

let rule_names_of_grammar grammar =
  List.fold_left
    (fun rnames (rname, _) -> Sset.add rname rnames)
    Sset.empty
    grammar

let leaves_of_grammar grammar =
  Sset.diff (names_of_grammar grammar) (rule_names_of_grammar grammar)

(** [completion g] adds rule of the form [name ::= ε] for each unbound symbol
    in the grammar *)
let completion grammar =
  let leaves =
    Sset.fold
      (fun leaf_name rules -> make_epsilon_rule leaf_name :: rules)
      (leaves_of_grammar grammar)
      []
  in
  grammar @ leaves

(** {2 Pretty printing} *)

let pp_elem fmt = function
  | Elem name -> Format.fprintf fmt "Elem(%s)" name
  | Seq name -> Format.fprintf fmt "Seq(%s)" name

let pp_product pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " * " in
  match terms with
  | [] -> Format.fprintf fmt "1"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp_component fmt (weight, elems) =
  if weight <> 0 then
    Format.fprintf fmt "<z^%d> * " weight;
  pp_product pp_elem fmt elems

let pp_union pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " + " in
  match terms with
  | [] -> Format.fprintf fmt "0"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp_rule fmt (name, components) =
  Format.fprintf fmt "%s ::= %a" name (pp_union pp_component) components

let pp fmt grammar =
  let pp_sep fmt () = Format.fprintf fmt "\n" in
  Format.pp_print_list ~pp_sep pp_rule fmt grammar

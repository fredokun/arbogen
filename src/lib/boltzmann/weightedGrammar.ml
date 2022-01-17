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

open Oracles.Types

type t = {rules: expression array; names: string array}
and expression = {weight: float; desc: expression_desc}
and expression_desc =
  | Z of int
  | Product of expression * expression
  | Union of expression * expression
  | Seq of expression
  | Ref of int

let product e1 e2 = {weight = e1.weight *. e2.weight; desc = Product (e1, e2)}
let union e1 e2 = {weight = e1.weight +. e2.weight; desc = Union (e1, e2)}
let seq e = {weight = 1. /. (1. -. e.weight); desc = Seq e}


(** {2 Conversion from grammars} *)

let of_expression oracle =
  let rec aux = function
    | Grammar.Z n -> {weight = oracle.z ** (float_of_int n); desc = Z n}
    | Grammar.Product (e1, e2) -> product (aux e1) (aux e2)
    | Grammar.Union (e1, e2) -> union (aux e1) (aux e2)
    | Grammar.Seq e -> seq (aux e)
    | Grammar.Ref i -> {weight = oracle.values.(i); desc = Ref i}
  in
  aux

let of_grammar oracle grammar =
  let rules = grammar.Grammar.rules in
  let names = grammar.Grammar.names in
  let rules = Array.map (of_expression oracle) rules in
  {names; rules}

(** {2 Pretty printing} *)

let pp_expression =
  let rec pp fmt {weight; desc} =
    Format.fprintf fmt "{%F; %a}" weight pp_desc desc
  and pp_desc fmt = function
    | Z n -> Format.fprintf fmt "z^%d" n
    | Product (e1, e2) -> Format.fprintf fmt "%a * %a" pp e1 pp e2
    | Union (e1, e2) -> Format.fprintf fmt "%a + %a" pp e1 pp e2
    | Seq e -> Format.fprintf fmt "Seq(%a)" pp e
    | Ref i -> Format.fprintf fmt "Ref(%d)" i
  in
  pp

let pp fmt {rules; names} =
  Array.iteri
    (fun i expr ->
       Format.fprintf fmt "%s ::= %a@\n" names.(i) pp_expression expr)
    rules

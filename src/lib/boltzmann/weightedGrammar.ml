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

type t = {rules: expression array; names: string array}

and expression =
  | Z of int
  | Product of expression * expression
  | Union of float * expression * expression
  | Seq of float * expression
  | Ref of int

(** {2 Conversion from grammars} *)

let of_expression (oracle: Oracle.t) =
  let rec aux = function
    | Grammar.Z n ->
      (Z n, oracle.z ** float_of_int n)
    | Grammar.Product (e1, e2) ->
      let e1, w1 = aux e1 and e2, w2 = aux e2 in
      (Product (e1, e2), w1 *. w2)
    | Grammar.Union (e1, e2) ->
      let e1, w1 = aux e1 and e2, w2 = aux e2 in
      let w = w1 +. w2 in
      (Union (w1 /. w, e1, e2), w)
    | Grammar.Seq e ->
      let e, w = aux e in
      (Seq (1. -. w, e), 1. /. (1. -. w))
    | Grammar.Ref i ->
      (Ref i, oracle.values.(i))
  in
  fun e -> fst (aux e)

let of_grammar oracle grammar =
  let rules = grammar.Grammar.rules in
  let names = grammar.Grammar.names in
  let rules = Array.map (of_expression oracle) rules in
  {names; rules}

(** {2 Pretty printing} *)

let pp_expression =
  let rec pp fmt = function
    | Z n ->
      Format.fprintf fmt "z^%d" n
    | Product (e1, e2) ->
      Format.fprintf fmt "%a * %a" pp e1 pp e2
    | Union (w, e1, e2) ->
      Format.fprintf fmt "Union (%F, %a, %a)" w pp e1 pp e2
    | Seq (w, e) ->
      Format.fprintf fmt "Seq(%F, %a)" w pp e
    | Ref i ->
      Format.fprintf fmt "Ref(%d)" i
  in
  pp

let pp fmt {rules; names} =
  Array.iteri
    (fun i expr ->
      Format.fprintf fmt "%s ::= %a@\n" names.(i) pp_expression expr )
    rules

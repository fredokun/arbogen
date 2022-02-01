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
  | Product of expression list
  | Union of (float * expression) list
  | Seq of float * expression
  | Ref of int

(** {2 Conversion from grammars} *)

let of_expression (oracle : Oracle.t) =
  let rec aux = function
    | Grammar.Z n ->
      (Z n, oracle.z ** float_of_int n)
    | Grammar.Product args ->
      let args, weight =
        List.fold_right
          (fun e (args, w) ->
            let e', w' = aux e in
            (e' :: args, w *. w') )
          args ([], 1.)
      in
      (Product args, weight)
    | Grammar.Union args ->
      let args, total =
        List.fold_right
          (fun e (args, tot) ->
            let e, w = aux e in
            ((w, e) :: args, tot +. w) )
          args ([], 0.)
      in
      (Union (List.map (fun (w, e) -> (w /. total, e)) args), total)
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
    | Product args ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " * ")
        pp fmt args
    | Union args ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " | ")
           pp_weighted_pair )
        args
    | Seq (w, e) ->
      Format.fprintf fmt "Seq(%F, %a)" w pp e
    | Ref i ->
      Format.fprintf fmt "Ref(%d)" i
  and pp_weighted_pair fmt (w, e) = Format.fprintf fmt "%F -> %a" w pp e in
  pp

let pp fmt {rules; names} =
  Array.iteri
    (fun i expr ->
      Format.fprintf fmt "%s ::= %a@\n" names.(i) pp_expression expr )
    rules

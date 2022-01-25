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

type t = {names: string array; rules: int expression array}

and 'ref expression =
  | Z of int
  | Product of 'ref expression * 'ref expression
  | Union of 'ref expression * 'ref expression
  | Seq of 'ref expression
  | Ref of 'ref

let product x y = Product (x, y)

let product_n = function
  | [] -> Z 0
  | x :: xs -> List.fold_left product x xs

let union x y = Union (x, y)

let union_n = function
  | [] -> invalid_arg "Grammar.union_n: empty unions are forbidden"
  | x :: xs -> List.fold_left union x xs

(* Pretty printing *)

let pp_expression ~pp_ref =
  let rec aux fmt = function
    | Product (e1, e2) ->
      Format.fprintf fmt "%a * %a" aux e1 aux e2
    | Union (e1, e2) ->
      Format.fprintf fmt "(%a + %a)" aux e1 aux e2
    | Seq e ->
      Format.fprintf fmt "Seq(%a)" aux e
    | Ref name ->
      pp_ref fmt name
    | Z i ->
      Format.fprintf fmt "z^%d" i
  in
  aux

let pp fmt {rules; names} =
  Array.iteri
    (fun i expr ->
      Format.fprintf fmt "%s ::= %a@\n" names.(i)
        (pp_expression ~pp_ref:Format.pp_print_int)
        expr )
    rules

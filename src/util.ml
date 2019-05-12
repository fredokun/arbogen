(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Util                                          *
 * -------                                               *
 * Misc. utilities                                       *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

(* List utilities *)

let fold_map mop fop finit a =
  List.fold_left (fun r e -> fop (mop e) r) finit a

let string_of_list_buf str_of_elem buf op dl cl l =
  let rec aux = function
    | [] -> Buffer.add_string buf cl
    | [e] ->
      Buffer.add_string buf op;
      str_of_elem e;
      Buffer.add_string buf cl;
    | e::l' ->
      Buffer.add_string buf op;
      str_of_elem e;
      Buffer.add_string buf dl;
      (aux l');
  in Buffer.add_string buf op;
  (aux l)

let rec output_list out output_elem op dl cl l = match l with
  | [] -> output_string out cl
  | [e] ->
    output_string out op ;
    output_elem out e ;
    output_string out cl
  | e::l' ->
    output_string out op ;
    output_elem out e ;
    output_string out dl ;
    output_list out output_elem "" dl cl l'

(* Array utilities *)

let array_for_all2 p u v =
  if Array.length u <> Array.length v then invalid_arg "array_for_all2";
  try
    Array.iter2 (fun x y -> if not (p x y) then raise Exit) u v;
    true
  with Exit -> false
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
      begin
        Buffer.add_string buf op;
        str_of_elem e;
        Buffer.add_string buf cl;
      end
    | e::l' ->
      begin
        Buffer.add_string buf op;
        str_of_elem e;
        Buffer.add_string buf dl;
        (aux l');
      end
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

let array_fold_left_2 (f:'a -> 'b -> 'c -> 'a) (init:'a) (b:'b array) (c:'c array) : 'a =
  let rec aux i len acc =
    if i=len
    then acc
    else aux (i+1) len (f acc b.(i) c.(i))
  in
  aux 0 (Array.length b) init

let array_for_all2 p u v =
  if Array.length u <> Array.length v then invalid_arg "array_for_all2";
  try
    Array.iter2 (fun x y -> if not (p x y) then raise Exit) u v;
    true
  with Exit -> false

(* string map *)

module StringMap = Map.Make(String)

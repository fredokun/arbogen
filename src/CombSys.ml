(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: CombSys                                       *
 * -------                                               *
 * Internal representation of a system of                *
 * combinatorial equations.                              *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *	     under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Grammar
open Util

(** {2 Internal representation of combinatorial systems} *)

(** a system is an array of equations *)
type combsys = combeq array

(** an equation is a list of products *)
and combeq = combprod list

(** a product is a list of nodes *)
and combprod = combnode list

(** a node is either *)
and combnode =
  | Z            (** an instance of the variable Z *)
  | One          (** a unit 1 for the product *)
  | Refe of int  (** a reference to another equation *)
  | Seq of int   (** a sequence construction *)

let combsys_size = Array.length

let eq sys1 sys2 = (sys1 = sys2)


(** {2 Evaluation of combinatorial systems} *)

(** evaluation of a node at a given coordinate z *)
let eval_combnode (z: float) (y: float array) = function
  | Z -> z
  | One -> 1.0
  | Refe i -> y.(i)
  | Seq i -> 1. /. (1. -. y.(i))

(** evaluation of a product at a given coordinate z *)
let eval_combprod (z: float) (y: float array) (cp: combprod) : float =
  let eval_combnode_s = eval_combnode z y in
  fold_map eval_combnode_s ( *. ) 1.0 cp

(** evaluation of an equation at a given coordinate z *)
let eval_eq (z: float) (y: float array) (eq: combeq) : float =
  let eval_combprod_s = eval_combprod z y in
  fold_map eval_combprod_s ( +. ) 0.0 eq

(** evaluation of a system at a given coordinate z *)
let evaluation (phi: combsys) (z: float) (y: float array) : float array =
  Array.init (combsys_size phi) (fun i -> eval_eq z y phi.(i))

let rec make_z = function
  | 0 -> [One]
  | n -> Z::(make_z ((-) n 1))

let rec make_refs map refs =
  match refs with
  | [] -> []
  | Grammar.Seq(a)::refs' -> (Seq (StringMap.find a map))::(make_refs map refs')
  | ref::refs' -> (Refe (StringMap.find (name_of_elem ref) map))::(make_refs map refs')

let comprod_of_component map (weight, refs) =
  (make_refs map refs) @ (make_z weight)

let combeq_of_rule map (_,comps) =
  List.fold_left (fun eqs comp -> (comprod_of_component map comp)::eqs) [] comps

let refmap_of_grammar grm =
  let rec aux i grm map = match grm with
    | [] -> map
    | (rname,_)::grm' -> aux (i+1) grm' (StringMap.add rname i map)
  in
  aux 0 grm StringMap.empty

let combsys_of_grammar grm =
  let rec aux grm map sys = match grm with
    | [] -> sys
    | ((rname,_) as rule)::grm' ->
      let index = StringMap.find rname map in
      Array.set sys index (combeq_of_rule map rule) ;
      aux grm' map sys
  in
  aux grm (refmap_of_grammar grm) (Array.make (List.length grm) [])


(** {2 Pretty-printing} *)

let pp_combnode fmt = function
  | Z -> Format.fprintf fmt "z"
  | One -> Format.fprintf fmt "1"
  | Seq i -> Format.fprintf fmt "Seq[%d]" i
  | Refe i -> Format.fprintf fmt "Ref[%d]" i

let pp_combprod fmt prod =
  let pp_sep fmt () = Format.fprintf fmt " * " in
  Format.pp_print_list ~pp_sep pp_combnode fmt prod

let pp_combeq fmt eq =
  let pp_sep fmt () = Format.fprintf fmt " + " in
  Format.pp_print_list ~pp_sep pp_combprod fmt eq

let pp fmt sys =
  Array.iteri (fun i eq -> Format.fprintf fmt "%d ==> %a\n" i pp_combeq eq) sys

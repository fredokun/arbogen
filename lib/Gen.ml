(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Gen                                           *
 * -------                                               *
 * The Boltzmann random generator                        *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Tree

open Util

open CombSys

open Grammar

open OracleSimple

let gen_tree (g:grammar) (with_prefix:bool) (idprefix:string) (sizemax:int) (y:float array) : (option Tree * int) =
  failwith "TODO: not implemented"

(* TODO: à documenter *)
let generator 
    (g:grammar)
    (self_seed:bool) (seed:int)
    (sizemin:int) (sizemax:int) 
    (epsilon1:float) (epsilon1_factor:float)
    (epsilon2:float) (epsilon2_factor:float)
    (with_prefix:bool) (idprefix:string) 
    (max_try:int) (ratio_rejected:float)
    (max_refine:int) : (tree*int) option =
  (if self_seed
   then Random.self_init
   else Random.init seed) ;
  let sys = combsys_of_grammar g in
  let rec gen epsilon1 epsilon2 zmin zmax nb_refine =
    let (zmin',zmax',y) = searchSingularity sys zmin zmax epsilon1 epsilon2 in
    let rec try_gen (nb_try:int) (nb_smaller:int) (nb_bigger:int) : (Some (Tree * int) * int * int) =
      if nb_try > 0 then
        (match gen_tree g with_prefix idprefix sizemax y with
          | (Some tree,size) -> 
            if size<sizemin then
              try_gen (nb_try-1) (nb_smaller+1) nb_bigger
            else (Some (tree,size), nb_smaller, nb_bigger)
          | (None,_) -> try_gen (nb_try-1) nb_smaller (nb_bigger+1))
      else (* max number of tries *)
        (None,nb_smaller,nb_bigger) 
    in
    if nb_refine>0 then
      (let (ptree,nb_smaller,nb_larger) = try_gen max_try 0 0 in
       match ptree with
         | Some _ -> ptree (* ok, found a tree, return it with its size *)
         | None -> 
           if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_larger)) >= ratio_rejected
           then (* if more than e.g. 80% of the trees are too small, then refine *)
             gen (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) zmin' zmax' (nb_refine+1))
    else (* refined too much : could not generate a tree *)
      (None,0) 

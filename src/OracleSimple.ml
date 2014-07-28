(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: OracleSimple                                  *
 * -------                                               *
 * The Oracle for the singularity search (naive version) *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open CombSys

open Util


let normInf_diff = array_fold_left_2 (fun norm y y' -> let z = abs_float (y -. y') in if z>norm then z else norm) 0.0

let iterationSimple (phi:combsys) (z:float) (epsilon:float):float array  =
  let rec iterate (y:float array): float array =
    (* print_endline "lol"; *)
    (* let open Printf in *)
    (* Array.iter (fun x -> printf "%f \n" x) y; *)
    let y' = evaluation phi z y
    in
    if (Array.fold_left (fun pred x -> pred || (x > 1.)) false y')
    then (Array.make (Array.length y') (-1.0))
    else
      if (normInf_diff y y') <= epsilon
      then y'
      else
	      iterate y'
  in
  iterate (Array.make (combsys_size phi) 0.0)

(* TODO: en une ligne avec array_exists *)
let diverge (y:float array) (epsilon:float):bool =
  let tooBig = 1.0/.epsilon in
  let rec dvgi (i:int) (s:int):bool =
    if i <= s then
      (if (y.(i) < 0.0) || (y.(i) > tooBig) then true
       else dvgi (i+1) s)
    else false
  in
  dvgi 0 ((Array.length y) - 1)

(* output:zmin,zmax,vectorY *)
let rec searchSingularity (phi:combsys) (zmin:float) (zmax:float) (epsilon1:float) (epsilon2:float)(zstart:float):float *float* float array =
  if zmax -. zmin < epsilon1 then
    (zmin,zmax,iterationSimple phi zmin epsilon2)
  else
    let z = zstart in
    let y = iterationSimple phi z epsilon2 in
    if diverge y epsilon2 = true then
      searchSingularity phi zmin zstart epsilon1 epsilon2 ((zmin+.zstart)/.2.)
    else
      searchSingularity phi zstart zmax epsilon1 epsilon2 ((zmax+.zstart)/.2.)

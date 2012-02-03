(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: OracleSimple                                  *
 * -------                                               *
 * The Oracle for the singularity search (naive version) *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open CombSys

open Util


let normInf_diff = array_fold_left_2 (fun norm y y' -> let z = abs_float (y -. y') in if z>norm then z else norm) 0.0 

let iterationSimple (phi:combsys) (z:float) (epsilon:float):float array  =
  let rec iterate (y:float array): float array = 
    let y' = evaluation phi z y
    in
	if normInf_diff y y' <= epsilon
    then y'
    else iterate y'
  in
  iterate (Array.make (combsys_size phi) 0.0)

(* TODO: en une ligne avec array_exists *)
let diverge (y:float array) (epsilon:float):bool =
  let tooBig = 1.0/.epsilon in 
  let rec dvgi (i:int) (s:int):bool = 
    (*print_endline ("element = " ^ (string_of_float ele));*)
    if i <= s then
      (if (y.(i) < 0.0) || (y.(i) > tooBig) then true
       else dvgi (i+1) s)
    else false
  in
  dvgi 0 ((Array.length y) - 1)

(* output:zmin,zmax,vectorY *)
let rec searchSingularity phi (zmin:float) (zmax:float) (epsilon1:float) (epsilon2:float):float *float* float array = 
	if zmax -. zmin < epsilon1 then
		(zmin,zmax,iterationSimple phi zmin epsilon2)
	else
		let z = (zmin +. zmax)/.2.0 in
		let y = iterationSimple phi z epsilon2 in
		(*print_endline ("singularite= " ^ (string_of_float zmin) ^ "moyenne= " ^ (string_of_float z));*)
		if diverge y epsilon2 = true then
			searchSingularity phi zmin z epsilon1 epsilon2
		else
			searchSingularity phi z zmax epsilon1 epsilon2
	
	 


 



(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: OracleSimple                                  *
 * -------                                               *
 * The Oracle for the singularity search (naive version) *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open CombSys

open Util


let iterationSimple (phi:combsys) (z:float) (epsilon:float):float array  =
	let thesize = get_length phi in
	
	let y' = Array.make thesize 0.0 in
	let thenorm = ref 0.0 in
	thenorm := epsilon +. 1.0; 

	let rec go_on ():float array = 
		let y = Array.copy y'
		in
		array_clone (evaluation phi z y) y';
		(*print_endline (string_of_array string_of_float y');*)
		let ymy' = array_soustrac y y'
		in
		let norm_inf = normI ymy'
		in
		if norm_inf > epsilon
		then go_on ()
		else 
		begin
		(*printVector y';*)
		y'
		end
  	in
	go_on ()

let diverge (y:float array) (epsilon:float):bool =
	let tresGrand = 1.0/.epsilon in 
	let rslt = ref false in
	let rec dvgi (i:int) (s:int):unit = 
		let ele = y.(i) in
			(*print_endline ("element = " ^ (string_of_float ele));*)
			if i < s then
				(
					if (ele < 0.0) || (ele > tresGrand) then
						rslt := true
					else
						dvgi (i+1) s
			
				)
	in
	dvgi 0 ((Array.length y) - 1);
	!rslt

(* output:xmin,xmax,vectorY *)
let rec rechercheSingularite phi (xmin:float) (xmax:float) (epsilon1:float) (epsilon2:float):float *float* float array = 
	if xmax -. xmin < epsilon1 then
		(xmin,xmax,iterationSimple phi xmin epsilon2)
	else
		let x = (xmin +. xmax)/.2.0 in
		let y = iterationSimple phi x epsilon2 in
		(*print_endline ("singularite= " ^ (string_of_float xmin) ^ "moyenne= " ^ (string_of_float x));*)
		if diverge y epsilon1 = true then 
			rechercheSingularite phi xmin x epsilon1 epsilon2
		else
			rechercheSingularite phi x xmax epsilon1 epsilon2;
	
(*
let rec rechercheSingularite phi (xmin:float) (xmax:float) (epsilon1:float) (epsilon2:float):float * float array = 

	if xmax -. xmin < epsilon1 then
		(xmin,iterationSimple phi xmin epsilon2)
	else
		let x = (xmin +. xmax)/.2.0 in
		let y = iterationSimple phi x epsilon2 in
		if diverge y epsilon1 = true then 
			rechercheSingularite phi xmin x epsilon1 epsilon2
		else
			rechercheSingularite phi x xmax epsilon1 epsilon2;
*)		 
		
		 


 



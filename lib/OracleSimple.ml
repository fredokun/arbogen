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


let array_sub = array_binop (-.) 0.0 ;;

let normI_diff = array_fold_left_2 (fun norm y y' -> let z = abs_float (y -. y') in if z>norm then z else norm) 0.0 

let normI = Array.fold_left (fun init e -> let ae = abs_float e in if ae  > init then ae else init) 0.0 

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
		let ymy' = array_sub y y'
		in
		let norm_inf = normI ymy'
		in
		if norm_inf > epsilon
		then go_on ()
		else 
		begin
		y'
		end
  	in
	go_on ()

let diverge (y:float array) (epsilon:float):bool =
  let tooBig = 1.0/.epsilon in 
  let rec dvgi (i:int) (s:int):bool = 
    let ele = y.(i) in
    (*print_endline ("element = " ^ (string_of_float ele));*)
    if i < s then
      (if (ele < 0.0) || (ele > tooBig) then true
       else dvgi (i+1) s)
    else false
  in
  dvgi 0 ((Array.length y) - 1)

(* output:xmin,xmax,vectorY *)
let rec searchSingularity phi (xmin:float) (xmax:float) (epsilon1:float) (epsilon2:float):float *float* float array = 
	if xmax -. xmin < epsilon1 then
		(xmin,xmax,iterationSimple phi xmin epsilon2)
	else
		let x = (xmin +. xmax)/.2.0 in
		let y = iterationSimple phi x epsilon2 in
		(*print_endline ("singularite= " ^ (string_of_float xmin) ^ "moyenne= " ^ (string_of_float x));*)
		if diverge y epsilon1 = true then 
			searchSingularity phi xmin x epsilon1 epsilon2
		else
			searchSingularity phi x xmax epsilon1 epsilon2
	
	 


 



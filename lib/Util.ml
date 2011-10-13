(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Util                                          *
 * -------                                               *
 * Misc. utilities                                       *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

(** option type for partial functions *)
type 'a option =
  | None
  | Some of 'a

(* List utilities *)

let fold_map mop fop finit a =
  List.fold_left (fun r e -> fop (mop e) r) finit a 

let rec string_of_list str_of_elem op dl cl l = match l with
  | [] -> cl
  | [e] -> (str_of_elem e) ^ cl
  | e::l' -> (string_of_elem e) ^ dl ^ (string_of_list op dl cl l')

(* Array utilities *)

let array_unop (op:'a -> 'b) (init:'b) (a:'a array) : 'b array =
  let rec aux i len b =
    if i<len
    then begin 
      b.(i) <- (op a.(i)) ;
      aux (i+1) len b
    end 
  in
  let n = Array.length a
  in
  let b = Array.make n init
  in 
	aux 0 n b;
	b

let array_binop (op:'a -> 'b -> 'c) (init:'c) (a:'a array) (b:'b array) : 'c array =
  let rec aux i len c =
    if i<len
    then begin 
      c.(i) <- (op a.(i) b.(i)) ;
      aux (i+1) len c
    end 
  in
  let n = Array.length a
  in
  let c = Array.make n init
  in 
	aux 0 n c; 
	c 

let string_of_array (tostr:'a -> string) (a : 'a array) : string =
	"[" ^ (Array.fold_left (fun str e -> str ^  (tostr e) ^ ",") "" a) ^ "]" 



(* modifié pour float on utilise > non >.*)
let array_max (a:float array) = Array.fold_left (fun (max:float) (e:float) -> if e > max then e else max) min_float a

let array_fold_left_2 (f:'a -> 'b -> 'c -> 'a) (init:'a) (b:'b array) (c:'c array) : 'a =
  let rec aux i len acc =
    if i=len 
    then acc
    else aux (i+1) len (f acc b.(i) c.(i))
  in
  aux 0 (Array.length b) init

let array_fold_it (f:int -> 'a -> 'b -> 'b) (tab:'a array) (unt:'b) : 'b = 
  let rec aux i max acc =
    if i=max then acc
    else aux (i+1) max (f i tab.(i) acc)
  in
  aux 0 ((Array.length tab)-1) unt ;;




let array_clone (a:'a array) (b:'a array) : unit =
	Array.blit a 0 b 0 (Array.length a)

(* string set *)

module StringSet = Set.Make (String) ;;

(* string map *)

module StringMap = Map.Make (String) ;;

let compareStrArr (s:string) (a:string array) : bool =
	let l = Array.length a in
	let theend = l - 1 in
	let r = ref false in
	for i = 0 to theend do
		if String.compare s a.(i) = 0 then
			 r := true
	done;
	!r

let findIndexstr (st:string array) (s:string) : int = 
	let l = Array.length st in
	let theend = l - 1 in
	let r = ref 100 in
	for i = 0 to theend do
		if String.compare s st.(i) = 0 then
			 r := i
	done;
	!r
	

let interval (i1:int)  (i2:int) (x:int) : bool =
	if (x >= i1) && (x <= i2) 
	then
		true
	else
		false

let sumTab (t : int array) : int =
	let l = Array.length t in
	let e = l - 1 in
	let r = ref 0 in
	for i = 0 to e do
		r := !r + t.(i)
        done;
	!r


(* 0 signifie rien, 1 signifie < à l'interval,-1 signifie > à l'interval *) 
let ifAtLeast8Smallerin10OthersBigger (p1:int) (p2:int) (tab:int array) : bool  =
	let restab = Array.make 10 0 in
	for i = 0 to 9 do
		if tab.(i) < p1 then
			restab.(i) <- 1
		else
			if tab.(i) > p2 then 
                        	restab.(i) <- -1 			
                        else 				
                                restab.(i) <- 0
        done;
        let thesumTab = sumTab restab in
        if thesumTab >= 6 then
                 true
        else
                 false	


(*
let bernoulli (z:float) : bool = 
	let rd = Random.float 1.0 in
	let afrd = string_of_float rd in
	if rd < z then
		true
	else
		false
 let r = bernoulli 0.5;;

let af = string_of_bool r ;;

print_endline af ;;

"false"


let r2 = bernoulli 0.5;;

let af2 = string_of_bool r2 ;;

print_endline af2 ;;

"true" *)

(*let gra_first ((st,ru):string*rule): string  = st; *)











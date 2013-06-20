
open Printf 

(* a type of binary trees with data on all nodes *)
type 'a bintree =
  BEmpty
| BNode of ('a * 'a bintree * 'a bintree) ;;

(* build (non-uniform) random trees non-tail recursively *)
                        
let rec append_bintree_rand (v:'a) (t:'a bintree) : 'a bintree =
  match t with
  | BEmpty -> BNode(v, BEmpty, BEmpty)
  | BNode (w, BEmpty, r) -> BNode (w, BNode (v, BEmpty, BEmpty), r)
  | BNode (w, l, BEmpty) -> BNode(w, l, BNode (v, BEmpty, BEmpty))
  | BNode (w, l, r) -> (match (Random.bool ()) with
                          | true -> BNode (w, append_bintree_rand v l, r)
                          | false -> BNode (w, l, append_bintree_rand v r)) ;;

let build_rand_int_btree (size:int) : int bintree =
  let rec aux n =
    if n <= size then append_bintree_rand n (aux (n+1))
    else BEmpty
  in
    aux 1 ;;

let build_rand_int_btree_it (size:int) : int bintree =
  let rec aux n t =
    if n <= size then aux (n+1) (append_bintree_rand n t)
    else t
  in
    aux 1 BEmpty ;;

let btree100 = build_rand_int_btree 100 ;;
let btree10000 = build_rand_int_btree 10000 ;;
let btree100000 = build_rand_int_btree 100000 ;;
let btree262066 = build_rand_int_btree 262066 ;; (* the biggest constructile tree without stack overflow *)
(* not constructible
let btree1000000 = build_rand_int_btree 1000000 ;;
*)

let btree100it = build_rand_int_btree_it 100 ;;
let btree10000it = build_rand_int_btree_it 10000 ;;
let btree100000it = build_rand_int_btree_it 100000 ;;
let btree262066it = build_rand_int_btree_it 262066 ;;
let btree1000000it = build_rand_int_btree_it 1000000 ;;
(* let btree2500000it = build_rand_int_btree_it 2500000 ;;
let btree5000000it = build_rand_int_btree_it 5000000 ;;
let btree10000000it = build_rand_int_btree_it 10000000 ;; *)

let time f x =
    let start = Sys.time ()
    in let res = f x
    in let stop = Sys.time ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in
       res  (* courtesy of StackOverflow/questions/9061421 *) ;;


let forget e = e ; ()

let rec size_of_btree (t:'a bintree) : int = match t with
  | BEmpty -> 0
  | BNode (_, l, r) -> (size_of_btree l) + (size_of_btree r) + 1;;

let sizes () =
  forget (size_of_btree btree100) ;
  forget (size_of_btree btree10000) ;
  forget (size_of_btree btree100000) ;
  forget (size_of_btree btree262066) ;
  forget (size_of_btree btree1000000it) ;
(*  forget (size_of_btree btree2500000it) ;
  forget (size_of_btree btree5000000it) ;
  forget (size_of_btree btree10000000it) ; *)
;;

time sizes () ;;

let size_of_btree_it (t:'a bintree) : (int * int) = 
  let rec aux n t ts ts_size max_size =
    match t with
    | BEmpty -> (match ts with
      | [] -> (n,max_size)
      | t'::ts' -> aux n t' ts' (ts_size-1) max_size) 
    | BNode (_, l, r) -> aux (n+1) l (r::ts) (ts_size+1) (max (ts_size+1) max_size)
      printf "size now = %d" (max (ts_size+1) max_size)
  in
    aux 0 t [] 0 0 ;;

let show_stack_size t =
  let (res, stack_size)  = size_of_btree_it t
  in printf "Size=%d StackSize=%d\n" res stack_size ;;

let sizes () =
  show_stack_size btree100 ;
  show_stack_size btree10000 ;
  show_stack_size btree100000 ;
  show_stack_size btree262066 ;
  show_stack_size btree1000000it  ;
(*  show_stack_size btree2500000it  ;
  show_stack_size btree5000000it  ;
  show_stack_size btree10000000it  *)
;;

time sizes () ;;

type binary = int list

let rec binary_succ (b:binary) : binary option = 
  match b with
  | [] -> None
  | 0::b' -> Some (1::b') 
  | 1::b' -> (match (binary_succ b') with
    | None -> None
    | Some b'' -> Some (0::b''))
  | _ -> failwith "invalid binary" 
;;

binary_succ [0] ;;
binary_succ [1] ;;
binary_succ [0;0] ;;
binary_succ [1;0] ;;
binary_succ [0;1] ;;
binary_succ [1;1] ;;

let rec insert_btree (v:'a) (t:'a bintree) (b:binary) : 'a bintree =
  match t with
  | BEmpty -> failwith "empty subtree"
  | BNode(w,l,r) ->
    (match b with
    | [] -> failwith "empty binary"
    | [0] ->  (match l with
      | BEmpty -> BNode(w,BNode (v, BEmpty, BEmpty), r)
      | _ -> failwith "incorrect left leaf")
    | [1] ->  (match r with
      | BEmpty -> BNode(w,l, BNode (v, BEmpty, BEmpty))
      | _ -> failwith "incorrect right leaf")
    | 0::b' -> BNode(w,insert_btree v l b',r)
    | 1::b' -> BNode(w,l,insert_btree v r b')
    | _ -> failwith "incorrect binary") ;;

let gen_binary (n:int) =
  let rec aux k b =
    if k=n then b else aux (k+1) (0::b)
  in
    aux 0 [] ;;

gen_binary 1 ;;
gen_binary 2 ;;
gen_binary 3 ;;

let build_complete_btree (v:'a) (depth:int) : 'a bintree =
  let rec gen (b:binary) (t:'a bintree) =
    let t' = insert_btree v t b 
    in
    match binary_succ b with
    | None -> t'
    | Some b' -> gen b' t'
  and aux (n:int) (t:'a bintree) =
    if n>depth then t
    else aux (n+1) (gen (gen_binary n) t)
  in
    aux 1 (BNode (v, BEmpty, BEmpty)) ;;

(build_complete_btree 0 1) ;;
(build_complete_btree 0 2) ;;
(build_complete_btree 0 3) ;;

size_of_btree (build_complete_btree 0 1) ;;
size_of_btree (build_complete_btree 0 2) ;;
size_of_btree (build_complete_btree 0 3) ;;
size_of_btree (build_complete_btree 0 4) ;;
size_of_btree (build_complete_btree 0 5) ;;

let complete_btree_5 = (build_complete_btree 0 5) ;;
let complete_btree_10 = (build_complete_btree 0 10) ;;
let complete_btree_15 = (build_complete_btree 0 15) ;;
let complete_btree_20 = (build_complete_btree 0 20) ;;
let complete_btree_21 = (build_complete_btree 0 21) ;;
let complete_btree_22 = (build_complete_btree 0 22) ;;
let complete_btree_23 = (build_complete_btree 0 23) ;;

let sizes () =
  forget (size_of_btree complete_btree_5) ;
  forget (size_of_btree complete_btree_10 ) ;
  forget (size_of_btree complete_btree_15 ) ;
  forget (size_of_btree complete_btree_20 ) ;
  forget (size_of_btree complete_btree_21 ) ;
  forget (size_of_btree complete_btree_22 ) ;
  forget (size_of_btree complete_btree_23 ) ;
;;

time sizes () ;;

let sizes () =
  show_stack_size complete_btree_5 ;
  show_stack_size complete_btree_10 ;
  show_stack_size complete_btree_15 ;
  show_stack_size complete_btree_20 ;
  show_stack_size complete_btree_21 ;
  show_stack_size complete_btree_22 ;
  show_stack_size complete_btree_23 ;
;;

time sizes () ;;

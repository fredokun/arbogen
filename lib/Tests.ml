(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Tests                                         *
 * -------                                               *
 * Generator tests (can be run in the top-level)         *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Tree;;

open CombSys ;;

open OracleSimple;;

open Util;;

open Gen;;
open Grammar;;


Random.self_init();;

let bintree = [ ("binnode",0, NonTerminal [ [ "leaf" ] ; [ "binnode"; "binnode" ] ]) ;
                ("leaf",1, Terminal) ] ;;

let tryGen grammar sizemin sizemax = 
  let pthetree = generateur grammar sizemin sizemax 0.000001 0.0000000001 "binnode"  0.0 1.0 (gra_toCombSys grammar)
  in  match pthetree with
    | None -> print_endline "pas d'arbre trouvé"
    | Some (tree,size) -> 
      tree_print tree 1;
      print_string "la taille de l'arbre:  ";
      print_endline (string_of_int size) ;;


tryGen bintree 100000 500000;;

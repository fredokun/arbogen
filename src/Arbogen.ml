open Printf

open Options

let version_str = "arbogen v0.20121006 (beta)"

let usage = "Usage: arbogen <opt> <specfile>.arb"
let banner = 
"                       v .   ._, |_  .,\n" ^
"                    `-._\\/  .  \\ /    |/_\n" ^
"  ARBOGEN                \\\\  _\\, y | \\//\n" ^
"  °°°°°°°          _\\_.___\\\\, \\\\/ -.\\||\n" ^
"  *Fast*             `7-,--.`._||  / / ,\n" ^
"    Uniform          /'     `-. `./ / |/_.'\n" ^
"      Random                   |    |//\n" ^
"        Tree                   |_    /\n" ^
"         Generator             |-   |\n" ^
"                               |   =|\n" ^
" (C) 2012 F.Peschanski et. al  |    |\n" ^
"------------------------------/ ,  . \\---------- (jg)\n" ;;


Arg.parse [
  ("-version", Arg.Unit (fun () -> printf "%s\n%!" version_str ; exit 0),
   "print version information");
  ("-interactive", Arg.Unit (fun () -> global_options.interactive_mode <- true),
   "activate interactive mode");
  ("-verbose", Arg.Int (fun n -> 
    if n < 0 then
      begin
        eprintf "Error: wrong verbosity level %d => must be positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.verbosity <- n),
   "<n> : set the verbosity level to <n>  (a positive integer)");
  ("-min", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong minimum size %d => must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.size_min <- n),
   "<n> : set the minimum size for the generated tree to <n> (a strictly positive integer)");
  ("-max", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong maximum size %d => must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.size_max <- n),
   "<n> : set the maximum size for the generated tree to <n> (a strictly positive integer)");
  ("-seed", Arg.Int (fun n -> 
    global_options.self_seed <- false ;
    global_options.random_seed <- n),
   "<n> : set the random generator seed to <n>");
  ("-eps1", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 1 parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else global_options.epsilon1 <- x),
   "<x> : set the epsilon for singularity search (a strictly positive float number)");
  ("-eps1_factor", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 1 factor parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else global_options.epsilon1_factor <- x),
   "<x> : set the refinement factor for epsilon in singularity search (a strictly positive float number)");
  ("-eps2", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 2 parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else global_options.epsilon2 <- x),
   "<x> : set the epsilon for Newton iteration (a strictly positive float number)");
  ("-eps2_factor", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 2 factor parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else global_options.epsilon2_factor <- x),
   "<x> : set the refinement factor for epsilon in Newton iteration (a strictly positive float number)");
]
  (fun arg ->
    if (String.compare global_options.grammar_file "")=0
    then global_options.grammar_file <- arg
    else (eprintf "Error: grammar file already set, argument '%s' rejected\n%!" arg ; exit 0))
  usage;
;;

if (global_options.verbosity) > 0
then printf "%s\n%!" banner;;

if (String.compare global_options.grammar_file "") == 0
then (eprintf "Error: grammar file not specified\n... arborting.\n%!"; exit 0) ;;

if (global_options.verbosity) > 0
then printf "Loading grammar file: %s\n%!" global_options.grammar_file

let grammar =
  GParser.parse_from_file global_options.grammar_file ;;

if (global_options.verbosity) > 0
then printf "==> Grammar file loaded\n%!" ;;


if (global_options.verbosity) > 0
then printf "Generating tree\n%!" ;;
 
let result =
  Gen.generator
    grammar
    global_options.self_seed
    global_options.random_seed
    global_options.size_min
    global_options.size_max
    global_options.epsilon1
    global_options.epsilon1_factor
    global_options.epsilon2
    global_options.epsilon2_factor
    global_options.with_prefix
    global_options.idprefix
    global_options.max_try
    global_options.ratio_rejected
    global_options.max_refine
    global_options.zstart
in match result with
  None ->
    eprintf "Error: no tree generated ==> try to use different parameters\n%!" ;
    exit 1
| Some (tree,size) ->
  if (global_options.verbosity) > 0
  then begin
    printf "==> Tree generated with size=%d\n%!" size ;
    printf "Saving file to 'tree.arb'\n%!" ;
    Tree.file_of_tree true global_options.with_prefix "tree.arb" tree ;
    printf "==> file saved\n%!"
  end


 

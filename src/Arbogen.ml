open Printf

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
"------------------------------/ ,  . \\---------- (jg)\n"

type options_record = {
  mutable grammar_file: string;
  mutable verbosity: int;
  mutable self_seed : bool;
  mutable random_seed: int;
  mutable size_min: int;
  mutable size_max: int;
  mutable epsilon1: float;
  mutable epsilon1_factor: float;
  mutable epsilon2: float;
  mutable epsilon2_factor: float;
  mutable with_prefix: bool;
  mutable idprefix: string;
  mutable max_try: int;
  mutable ratio_rejected: float;
  mutable max_refine: int;
  mutable zstart: float;
} ;;

let global_options = {
  grammar_file = "";
  verbosity = 1;
  self_seed = true;
  random_seed = 1234;
  size_min = 10;
  size_max = 100;
  epsilon1 = 0.001;
  epsilon1_factor = 0.1;
  epsilon2 = 0.0001;
  epsilon2_factor = 0.1;
  with_prefix = false;
  idprefix =  "";
  max_try = 100;
  ratio_rejected = 0.8;
  max_refine = 6;
  zstart = 0.1;
} ;;

Arg.parse [
  ("-version", Arg.Unit (fun () -> printf "%s\n%!" version_str ; exit 0),
   "print version information");
  ("-seed", Arg.Int (fun n -> 
    global_options.self_seed <- false ;
    global_options.random_seed <- n),
   "<n> : set the random generator seed to <n>");
  ("-verbose", Arg.Int (fun n -> 
    if n < 0 then
      begin
        eprintf "Error: wrong verbosity level %d : must be positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.verbosity <- n),
   "<n> : set the verbosity level to <n>  (a positive integer)");
  ("-min", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong minimum size %d : must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.size_min <- n),
   "<n> : set the minimum size for the generated tree to <n> (a strictly positive integer)");
  ("-max", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong maximum size %d : must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else global_options.size_max <- n),
   "<n> : set the maximum size for the generated tree to <n> (a strictly positive integer)")
]
  (fun arg -> printf "Grammar file: %s (... not yet implemented...)\n%!" arg ; exit 0)
  usage;
;;

printf "%s\n%!" banner;;


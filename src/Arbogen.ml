open Printf

open Options


let version_str = "arbogen v0.20121006 (beta)"

let usage = "Usage: arbogen <opt> <specfile>.arb"
let banner = "\n" ^
"   A      ...:'....:'':...':......\n" ^
"   R    :''   ._   .  `.  \\   ,   '':\n" ^
"   B    ':  .   \" .|    \\  `>/   _.-': \n" ^
"   O   .:'  .`'.   `-.  '. /'  ,..  .:\n" ^
"   G  :'        `.    `\\| \\./   ' :\n" ^
"   E  :. ,,-'''''  \"-.   |   | ....:\n" ^
"   N   '.      ..'''  `\\ :   |\n" ^
"         ''''''''       \\'   |\n" ^
"*fast* uniform random    |  =|\n" ^
"       tree generator    |   |\n" ^
"                         |-  |\n" ^
"'''''''''''''''''''''''''''''''''''''''\n" ^ 
"(C) F. Peschanski et al. under the GPL\n";;

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
    else
      global_options.verbosity <- n),
   "<n> : set the verbosity level to <n>  (a positive integer)");
  ("-min", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong minimum size %d => must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else begin
      global_options.size_min <- n;
      global_options.size_min_set <- true
    end),
   "<n> : set the minimum size for the generated tree to <n> (a strictly positive integer)");
  ("-max", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong maximum size %d => must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else if n < global_options.size_min then
      begin 
        eprintf "Error: wrong maximum size %d => must be strictly bigger than minimum\n...aborting\n%!" n ;
        exit 1;
      end
    else  begin
      global_options.size_max <- n;
      global_options.size_max_set <- true
    end),
   "<n> : set the maximum size for the generated tree to <n> (a strictly positive integer)");
  ("-seed", Arg.Int (fun n ->
    begin
      global_options.self_seed <- false ;
      global_options.random_seed <- n;
      global_options.random_seed_set <- true
    end),
   "<n> : set the random generator seed to <n>");
  ("-eps1", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 1 parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.epsilon1 <- x;
      global_options.epsilon1_set <- true
    end),
   "<x> : set the epsilon for singularity search (a strictly positive float number)");
  ("-eps1_factor", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 1 factor parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.epsilon1_factor <- x;
      global_options.epsilon1_factor_set <- true
    end),
   "<x> : set the refinement factor for epsilon in singularity search (a strictly positive float number)");
  ("-eps2", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 2 parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.epsilon2 <- x;
      global_options.epsilon2_set <- true
    end),
   "<x> : set the epsilon for Newton iteration (a strictly positive float number)");
  ("-eps2_factor", Arg.Float (fun x ->
    if x < 0.0 then
      begin
        eprintf "Error: wrong epsilon 2 factor parameter %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.epsilon2_factor <- x;
      global_options.epsilon2_factor_set <- true
    end),
   "<x> : set the refinement factor for epsilon in Newton iteration (a strictly positive float number)");
  ("-reject_ratio", Arg.Float (fun x ->
    if x <  0.0 then
      begin
        eprintf "Error: wrong ratio of rejection %f => must be positive\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.ratio_rejected <- x;
      global_options.ratio_rejected_set <- true
    end),
   "<x> : set the rejection's ratio (a strictly positive float number)");
  ("-max_refine", Arg.Int (fun x ->
    if x <= 0 then
      begin
        eprintf "Error: wrong maximum number of refinement %d => must be a strictly positive integer number\n...aborting\n%!" x ;
        exit 1;
      end
    else begin
      global_options.max_refine <- x;
      global_options.max_refine_set <- true
    end),
   "<n> : set the refinement's maximum number of the Newton's parameters (a strictly positive integer number)");
  (* ("-max_refine_seed", Arg.Int (fun x -> *)
  (*   if x <= 0 then *)
  (*     begin *)
  (*       eprintf "Error: wrong maximum number of refinement %d => must be a strictly positive integer number\n...aborting\n%!" x ; *)
  (*       exit 1; *)
  (*     end *)
  (*   else begin *)
  (*     global_options.max_refine_seed <- x; *)
  (*     global_options.max_refine_seed_set <- true *)
  (*   end), *)
  (*  "<n> : set the maximum number of refinement of the seed before the refinement of the Newton's parameter (a strictly positive integer number)"); *)
  ("-try", Arg.Int (fun n ->
    if n <= 0 then
      begin
        eprintf "Error: wrong try number %d => must be strictly positive\n...aborting\n%!" n ;
        exit 1;
      end
    else begin
      global_options.max_try <- n;
      global_options.max_try_set <- true
    end),
   "<n> : set the maximum of tries when generating trees");
  ("-type", Arg.String(fun x ->
    match x with
      |"arb" -> global_options.output_type <- 0;
      |"dot" -> global_options.output_type <- 1;
      |"xml" -> global_options.output_type <- 2;
      |"all" -> global_options.output_type <- 3;  
      |_ -> eprintf "Error: wrong option value must be strictly arb,dot,xml or all\n...aborting\n";
            exit 1;
  ),
  "<n>: set the type [arb|dot|xml|all] of output generated at the end");
  ("-file",Arg.String(fun x->
    global_options.fileName <- x;
    ),
  "<x>: set the name of the file to be created at end of execution"
  );
  ("-zstart", Arg.Float(fun x -> 
    if(x > 1.0 || x < 0.0) then(
            eprintf "Error: value must be between 0 and 1\n...arborting\n";
            exit 1;
    )else(
      global_options.zstart <- x;
      global_options.zstart_set <- true;
    )
  ),
  "<x>: sets the value of zstart");
]
  (fun arg ->
    if (String.compare global_options.grammar_file "") == 0
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

let (options, ast_grammar) = ParseUtil.parse_from_file global_options.grammar_file ;;
ParseUtil.set_options options ;;
let grammar = Ast.grammar_of_ast_grammar ast_grammar ;;

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
     match global_options.output_type with 
     |0 -> printf "Saving file to '%s.arb'\n%!" global_options.fileName; 
           Tree.file_of_tree true global_options.with_prefix (global_options.fileName^".arb") tree; 
     |1 -> printf "Saving file to '%s.dot'\n%!" global_options.fileName; 
           Tree.file_of_dot true (global_options.fileName^".dot") tree;  
     |2 -> printf "Saving file to '%s.xml'\n%!" global_options.fileName; 
           Tree.file_of_xml (global_options.fileName^".xml") tree; 
     |3 -> printf "Saving files to '%s.arb' , '%s.dot' and '%s.xml'\n%!" global_options.fileName global_options.fileName global_options.fileName; 
           Tree.file_of_tree true global_options.with_prefix (global_options.fileName^".arb") tree; 
           Tree.file_of_dot true (global_options.fileName^".dot") tree; 
           Tree.file_of_xml (global_options.fileName^".xml") tree;  
     |_ -> printf "Error \n";      (* unreachable case *) 
          printf "==> file saved\n%!"
   end  

 

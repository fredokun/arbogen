(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Arbogen                                       *
 * -------                                               *
 * Main module and Argument parser                       *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Printf

open Arbolib
open Options
open GenState

let () =

  let version_str = "arbogen v0.20121006 (beta)" in

  let usage = "Usage: arbogen <opt> <specfile>.spec" in

  let banner = "
                A      ...:'....:'':...':......
                R    :''   ._   .  `.  \\   ,   '':
                B    ':  .   \" .|    \\  `>/   _.-':
                O   .:'  .`'.   `-.  '. /'  ,..  .:
                G  :'        `.    `\\| \\./   ' :
                E  :. ,,-'''''  \"-.   |   | ....:
                N   '.      ..'''  `\\ :   |
                      ''''''''       \\'   |
             *fast* uniform random    |  =|
                    tree generator    |   |
                                      |-  |
                '''''''''''''''''''''''''''''''''''''''
                (C) F. Peschanski et al. under the GPL\n" in


  Arg.parse [
      ("-version", Arg.Unit
                     (fun () -> printf "%s\n%!" version_str ; exit 0),
       "print version information");
      ("-interactive", Arg.Unit
                         (fun () -> global_options.interactive_mode <- true),
       "activate interactive mode");
      ("-verbose", Arg.Int
                     (fun n ->
                      if n < 0 then
                        begin
                          eprintf "Error: wrong verbosity level %d => must be positive\n...aborting\n%!" n ;
                          exit 1;
                        end
                      else
                        global_options.verbosity <- n),
       "<n> : set the verbosity level to <n>  (a positive integer)");
      ("-v", Arg.Int
               (fun n ->
                if n < 0 then
                  begin
                    eprintf "Error: wrong verbosity level %d => must be positive\n...aborting\n%!" n ;
                    exit 1;
                  end
                else
                  global_options.verbosity <- n),
       "<n> : same as -verbose <n>");
      ("-min", Arg.Int
                 (fun n ->
                  if n <= 0 then
                    begin
                      eprintf "Error: wrong minimum size %d => must be strictly positive\n...aborting\n%!" n ;
                      exit 1;
                    end
                  else begin
                      global_options.size_min <- n;
                      global_options.size_min_set <- true;
                      global_options.size_max <- (n + 10); (* Re-up default maximum *)
                    end),
       "<n> : set the minimum size for the generated tree to <n> (a strictly positive integer)");
      ("-max", Arg.Int
                 (fun n ->
                  if n <= 0 then
                    begin
                      eprintf "Error: wrong maximum size %d => must be strictly positive\n...aborting\n%!" n ;
                      exit 1;
                    end
                  else if n <= global_options.size_min then
                    begin
                      eprintf "Error: wrong maximum size %d => must be strictly bigger than minimum\n...aborting\n%!" n ;
                      exit 1;
                    end
                  else begin
                      global_options.size_max <- n;
                      global_options.size_max_set <- true
                    end),
       "<n> : set the maximum size for the generated tree to <n> (a strictly positive integer)");
      ("-seed", Arg.Int (fun n -> ParseUtil.set_option ~preserve:false "seed" (Vint n)),
       "<n> : set the random generator seed to <n>");
      ("-eps1", Arg.Float
                  (fun x ->
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
      ("-eps1_factor", Arg.Float
                         (fun x ->
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
      ("-eps2", Arg.Float
                  (fun x ->
                   if x < 0.0 then
                     begin
                       eprintf "Error: wrong epsilon 2 parameter %f => must be positive\n...aborting\n%!" x ;
                       exit 1;
                     end
                   else begin
                       global_options.epsilon2 <- x;
                       global_options.epsilon2_set <- true
                     end),
       "<x> : set the epsilon for simple iteration (a strictly positive float number)");
      ("-eps2_factor", Arg.Float
                         (fun x ->
                          if x < 0.0 then
                            begin
                              eprintf "Error: wrong epsilon 2 factor parameter %f => must be positive\n...aborting\n%!" x ;
                              exit 1;
                            end
                          else begin
                              global_options.epsilon2_factor <- x;
                              global_options.epsilon2_factor_set <- true
                            end),
       "<x> : set the refinement factor for epsilon in simple iteration (a strictly positive float number)");
      ("-reject_ratio", Arg.Float
                          (fun x ->
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
      ("-max_refine", Arg.Int
                        (fun x ->
                         if x <= 0 then
                           begin
                             eprintf "Error: wrong maximum number of refinement %d => must be a strictly positive integer number\n...aborting\n%!" x ;
                             exit 1;
                           end
                         else begin
                             global_options.max_refine <- x;
                             global_options.max_refine_set <- true
                           end),
       "<n> : set the refinement's maximum number of the simpel iteration's parameters (a strictly positive integer number)");
      ("-try", Arg.Int
                 (fun n ->
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
      ("-otype", Arg.String
                   (fun x ->
                    match x with
                    |"arb" -> global_options.output_type <- 0;
                    |"dot" -> global_options.output_type <- 1;
                    |"xml" -> global_options.output_type <- 2;
                    |"all" -> global_options.output_type <- 3;
                    |_ -> eprintf "Error: wrong option value must be strictly arb,dot,xml or all\n...aborting\n";
                          exit 1;
                   ),
       "<n>: set the type [arb|dot|xml|all] of the generated tree");
      ("-o",Arg.String
              (fun x->
               global_options.fileName <- x;
              ),
       "<x>: set the name of the file to be created at end of execution"
      );
      ("-zstart", Arg.Float
                    (fun x ->
                     if(x > 1.0 || x < 0.0) then(
                       eprintf "Error: value must be between 0 and 1\n...arborting\n";
                       exit 1;
                     )else(
                       global_options.zstart <- x;
                       global_options.zstart_set <- true;
                     )
                    ),
       "<x>: sets the value of zstart");

      ("-state", Arg.String
                   (fun x ->
                    global_options.state_file <- x;
                    global_options.with_state <- true;
                   ),
       "<n>: set the name of state file");
      ("-id", Arg.Unit
         (fun () ->
                 global_options.with_id <- true;
                ),
       ": number the nodes");
      ("-typ", Arg.Unit
         (fun () ->
                  global_options.with_type <- true;
                 ),
       ": show the type of nodes");
      ("-randgen", Arg.String
                     (fun x ->
                      match x with
                      | "ocaml" -> global_options.randgen <- "ocaml"
                      | "randu" -> global_options.randgen <- "randu"
                      | "randnull" -> global_options.randgen <- "randnull"
                      | _  -> (eprintf "Error : random number generator %s unknown\n%!" x; exit 1)),
       "[ocaml | randu] : set the random number generator");
      ("-oracle", Arg.Unit
         (fun () ->
            global_options.print_oracle <- true;
         ),
       ": output an oracle");
      ("-indent", Arg.Unit
         (fun () ->
                     global_options.indent <- true;
                    ),
       ": indent the output")
    ]
            (fun arg ->
             if (String.compare global_options.grammar_file "") = 0
             then global_options.grammar_file <- arg
             else (eprintf "Error: grammar file already set, argument '%s' rejected\n%!" arg ; exit 1))
            usage;

  if global_options.print_oracle then
    begin
      let (options, grammar) = ParseUtil.parse_from_file global_options.grammar_file in
      ParseUtil.set_options ~preserve:true options;
      let g = Grammar.completion grammar in
      Format.printf "[GRAMMAR]: grammar parsed is :\n%a@." Grammar.pp g;
      let sys = CombSys.combsys_of_grammar (Grammar.completion g) in
      Format.printf "[COMBSYS]: combinatorial system is:\n%a@." CombSys.pp sys;

      let zmin, zmax, zstart, epsilon1, epsilon2 = 0., 1., global_options.zstart,
                                                   global_options.epsilon1,
                                                   global_options.epsilon2 in
      Format.printf "[ORACLE]: search singularity at z=%F@." zstart;
      let oracle_config = OracleSimple.{epsilon1; epsilon2; zmin; zmax; zstart} in
      let (zmin', _, y) = OracleSimple.searchSingularity oracle_config sys in
      Format.printf "          ==> found singularity at z=%F@." zmin';
      let wgrm = WeightedGrammar.weighted_grm_of_grm g y zmin' in
      Format.printf "[ORACLE]: weighted grammar is :@\n%a@." WeightedGrammar.pp wgrm;
      exit 0
    end;

  if (global_options.verbosity) > 0 then
    printf "%s\n%!" banner;

  let result =
    if(not global_options.with_state) then
      begin
        if (global_options.verbosity) > 0 then
          printf "Loading grammar file: %s\n%!" global_options.grammar_file;

        let (options, grammar) = ParseUtil.parse_from_file global_options.grammar_file in
        ParseUtil.set_options ~preserve:true options;
        let grammar = Grammar.completion grammar in

        if (global_options.verbosity) > 0 then
          printf "==> Grammar file loaded\n%!";

        if (global_options.verbosity) > 0 then
          printf "Generating tree\n%!";

        if (global_options.verbosity) > 0 then
          printf "Random number generator used  is %s\n%!" global_options.randgen;

        Gen.generator
          grammar
          ~seed:global_options.random_seed
          global_options.size_min
          global_options.size_max
          global_options.epsilon1
          global_options.epsilon1_factor
          global_options.epsilon2
          global_options.epsilon2_factor
          global_options.max_try
          global_options.ratio_rejected
          global_options.max_refine
          global_options.zstart
	        global_options.randgen
	       global_options.verbosity
      end
    else
      begin

        if (global_options.verbosity) > 0 then
          printf "Loading state file: %s\n%!" global_options.state_file;

        let in_channel = open_in global_options.state_file in
        let state:gen_state = input_value in_channel in
        close_in in_channel;

        global_options.randgen <- state.randgen;

        if (global_options.verbosity) > 0 then
          printf "==> State file loaded\n%!";

        if (global_options.verbosity) > 0 then
          printf "Generating tree\n%!";

        if (global_options.verbosity) > 0 then
          printf "Random number generator used  is %s\n%!" global_options.randgen;

        let (tree,size) = Gen.gen_tree state in
        Some(tree,size,state)
      end
  in
  match result with
  | None ->
     eprintf "Error: no tree generated ==> try to use different parameters\n%!" ;
     exit 1
  | Some (tree,size,state) ->
     begin

       if global_options.verbosity > 0 then
         printf "==> Tree generated with %d nodes\n%!" size;

       let out_state =
         if global_options.fileName = "" then
           begin
             if global_options.verbosity >= 2 then
               printf "==> Saving state to file 'state'\n%!" ;
             open_out "state"
           end
         else
           begin
             if global_options.verbosity >= 2 then
               printf "==> Saving state to file '%s.state'\n%!" global_options.fileName;
             open_out (global_options.fileName^".state")
           end
       in

       output_value out_state state;
       close_out out_state;

       match global_options.output_type with
       |0 ->
         let out =
           if global_options.fileName = "" then
             stdout
           else
             begin
               printf "Saving file to '%s.arb'\n%!" global_options.fileName;
               open_out (global_options.fileName^".arb")
             end
         in
         Tree.file_of_tree global_options.with_type global_options.with_id tree out;
       |1 ->
         let out =
           if global_options.fileName = "" then
             stdout
           else
             begin
               printf "Saving file to '%s.dot'\n%!" global_options.fileName;
               open_out (global_options.fileName^".dot")
             end
         in
         Tree.file_of_dot global_options.with_type global_options.with_id global_options.indent tree out;
       |2 ->
         let out =
           if global_options.fileName = "" then
             stdout
           else
             begin
               printf "Saving file to '%s.xml'\n%!" global_options.fileName;
               open_out (global_options.fileName^".xml")
             end
         in
         Tree.file_of_xml global_options.with_type global_options.with_id global_options.indent tree out;
       |3 ->
         if global_options.fileName = "" then
           global_options.fileName <- "tree";
         printf "Saving files to '%s.arb', '%s.dot' and '%s.xml'\n%!" global_options.fileName global_options.fileName global_options.fileName;
         Tree.file_of_tree global_options.with_type global_options.with_id tree (open_out (global_options.fileName^".arb"));
         Tree.file_of_dot global_options.with_type global_options.with_id global_options.indent tree (open_out (global_options.fileName^".dot"));
         Tree.file_of_xml global_options.with_type global_options.with_id global_options.indent tree (open_out (global_options.fileName^".xml"));
       |_ -> printf "Error \n";      (* unreachable case *)

       printf "==> file saved\n%!";
       exit 0
     end

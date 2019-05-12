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

open Arbolib
open Options
open GenState

let version_str = "arbogen v0.20121006 (beta)"
let usage = "Usage: arbogen <opt> <specfile>.spec"
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
              (C) F. Peschanski et al. under the GPL\n"

let speclist =
  let set_verbosity n =
    if n < 0 then begin
      Format.eprintf "Error: wrong verbosity level %d => must be positive@." n;
      exit 1
    end else global_options.verbosity <- n
  in

  [("-version", Arg.Unit (fun () -> Format.printf "%s@." version_str; exit 0),
    "print version information");

   ("-interactive", Arg.Unit (fun () -> global_options.interactive_mode <- true),
    "activate interactive mode");

   ("-verbose", Arg.Int set_verbosity,
    "<n> : set the verbosity level to <n>  (a non-negative positive integer)");

   ("-v", Arg.Int set_verbosity,
    "<n> : same as -verbose <n>");

   ("-min", Arg.Int (fun n -> ParseUtil.set_option "min" (Vint n)),
    "<n> : set the minimum size for the generated tree to <n> (a non-negative integer)");

   ("-max", Arg.Int (fun n -> ParseUtil.set_option "max" (Vint n)),
    "<n> : set the maximum size for the generated tree to <n> (a non-negative integer)");

   ("-seed", Arg.Int (fun n -> ParseUtil.set_option "seed" (Vint n)),
    "<n> : set the random generator seed to <n>");

   ("-eps1", Arg.Float (fun f -> ParseUtil.set_option "eps1" (Vfloat f)),
    "<x> : set the epsilon for singularity search (a positive float number)");

   ("-eps1_factor", Arg.Float (fun f -> ParseUtil.set_option "eps1_factor" (Vfloat f)),
    "<x> : set the refinement factor for epsilon in singularity search (a positive float number)");

   ("-eps2", Arg.Float (fun f -> ParseUtil.set_option "eps2" (Vfloat f)),
    "<x> : set the epsilon for simple iteration (a positive float number)");

   ("-eps2_factor", Arg.Float (fun f -> ParseUtil.set_option "eps2_factor" (Vfloat f)),
    "<x> : set the refinement factor for epsilon in simple iteration (a positive float number)");

   ("-reject_ratio", Arg.Float (fun f -> ParseUtil.set_option "reject_ratio" (Vfloat f)),
    "<x> : set the rejection's ratio (a positive float number)");

   ("-max_refine", Arg.Int (fun n -> ParseUtil.set_option "max_refine" (Vint n)),
    "<n> : set the refinement's maximum number of the simpel iteration's parameters (a positive integer number)");

   ("-try", Arg.Int (fun n -> ParseUtil.set_option "try" (Vint n)),
    "<n> : set the maximum of tries when generating trees");

   ("-otype", Arg.String (function
        | "arb" -> global_options.output_type <- 0;
        | "dot" -> global_options.output_type <- 1;
        | "xml" -> global_options.output_type <- 2;
        | "all" -> global_options.output_type <- 3;
        | _ ->
          Format.eprintf "Error: otype must be in [arb|dot|xml|all]@.";
          exit 1),
    "<n>: set the type [arb|dot|xml|all] of the generated tree");

   ("-o",Arg.String (fun x -> global_options.fileName <- x),
    "<x>: set the name of the file to be created at end of execution");

   ("-zstart", Arg.Float (fun f -> ParseUtil.set_option "zstart" (Vfloat f)),
    "<x>: sets the value of zstart");

   ("-state", Arg.String (fun x ->
        global_options.state_file <- x;
        global_options.with_state <- true;
      ),
    "<n>: set the name of state file");

   ("-id", Arg.Unit (fun () -> global_options.with_id <- true),
    ": number the nodes");

   ("-typ", Arg.Unit (fun () -> global_options.with_type <- true),
    ": show the type of nodes");

   ("-randgen", Arg.String (fun s -> ParseUtil.set_option "randgen" (Vstring s)),
    "[ocaml|randu|randnull] : set the random number generator");

   ("-oracle", Arg.Unit (fun () -> global_options.print_oracle <- true),
    ": output an oracle");

   ("-indent", Arg.Unit (fun () -> global_options.indent <- true),
    ": indent the output")
  ]

let () =
  Arg.parse speclist
    (fun arg ->
       if global_options.grammar_file = ""
       then global_options.grammar_file <- arg
       else (Format.eprintf "Error: grammar file already set, argument '%s' rejected@." arg ; exit 1))
    usage;
  ParseUtil.extra_checks ();

  if global_options.print_oracle then
    begin
      let (options, grammar) = ParseUtil.parse_from_file global_options.grammar_file in
      ParseUtil.set_options ~preserve:true options;
      Format.printf "[GRAMMAR]: grammar parsed is :\n%a@." Grammar.pp grammar;
      let zmin, zmax, zstart, epsilon1, epsilon2 = 0., 1., global_options.zstart,
                                                   global_options.epsilon1,
                                                   global_options.epsilon2 in
      Format.printf "[ORACLE]: search singularity at z=%F@." zstart;
      let oracle_config = OracleSimple.{epsilon1; epsilon2; zmin; zmax; zstart} in
      let (zmin', _, y) = OracleSimple.searchSingularity oracle_config grammar in
      Format.printf "          ==> found singularity at z=%F@." zmin';
      let wgrm = WeightedGrammar.of_grammar zmin' y grammar in
      Format.printf "[ORACLE]: weighted grammar is :@\n%a@." WeightedGrammar.pp wgrm;
      exit 0
    end;

  if (global_options.verbosity) > 0 then Format.printf "%s@." banner;

  let result =
    if(not global_options.with_state) then
      begin
        if (global_options.verbosity) > 0 then
          Format.printf "Loading grammar file: %s@." global_options.grammar_file;

        let (options, grammar) = ParseUtil.parse_from_file global_options.grammar_file in
        ParseUtil.set_options ~preserve:true options;

        if (global_options.verbosity) > 0 then
          Format.printf "==> Grammar file loaded@.";

        if (global_options.verbosity) > 0 then
          Format.printf "Generating tree@.";

        if (global_options.verbosity) > 0 then
          Format.printf "Random number generator used  is %s@." global_options.randgen;

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
          Format.printf "Loading state file: %s@." global_options.state_file;

        let in_channel = open_in global_options.state_file in
        let state:gen_state = input_value in_channel in
        close_in in_channel;

        global_options.randgen <- state.randgen;

        if (global_options.verbosity) > 0 then
          Format.printf "==> State file loaded@.";

        if (global_options.verbosity) > 0 then
          Format.printf "Generating tree@.";

        if (global_options.verbosity) > 0 then
          Format.printf "Random number generator used  is %s@." global_options.randgen;

        let module Rand = (val RandGen.get state.randgen) in
        Rand.set_state state.rnd_state;
        let tree, size = Gen.gen (module Rand) state.weighted_grammar in
        Some (tree, size, state)
      end
  in
  match result with
  | None ->
    Format.eprintf "Error: no tree generated ==> try to use different parameters@." ;
    exit 1
  | Some (tree,size,state) ->
    begin

      if global_options.verbosity > 0 then
        Format.printf "==> Tree generated with %d nodes@." size;

      let out_state =
        if global_options.fileName = "" then
          begin
            if global_options.verbosity >= 2 then
              Format.printf "==> Saving state to file 'state'@." ;
            open_out "state"
          end
        else
          begin
            if global_options.verbosity >= 2 then
              Format.printf "==> Saving state to file '%s.state'@." global_options.fileName;
            open_out (global_options.fileName^".state")
          end
      in

      output_value out_state state;
      close_out out_state;

      (* XXX. ugly workaround *)
      let tree = Tree.annotate tree in

      begin match global_options.output_type with
        |0 ->
          let out =
            if global_options.fileName = "" then
              stdout
            else
              begin
                Format.printf "Saving file to '%s.arb'@." global_options.fileName;
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
                Format.printf "Saving file to '%s.dot'@." global_options.fileName;
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
                Format.printf "Saving file to '%s.xml'@." global_options.fileName;
                open_out (global_options.fileName^".xml")
              end
          in
          Tree.file_of_xml global_options.with_type global_options.with_id global_options.indent tree out;
        |3 ->
          if global_options.fileName = "" then
            global_options.fileName <- "tree";
          Format.printf "Saving files to '%s.arb', '%s.dot' and '%s.xml'@." global_options.fileName global_options.fileName global_options.fileName;
          Tree.file_of_tree global_options.with_type global_options.with_id tree (open_out (global_options.fileName^".arb"));
          Tree.file_of_dot global_options.with_type global_options.with_id global_options.indent tree (open_out (global_options.fileName^".dot"));
          Tree.file_of_xml global_options.with_type global_options.with_id global_options.indent tree (open_out (global_options.fileName^".xml"));
        |_ -> failwith "unreachable case" end;

      Format.printf "==> file saved@.";
      exit 0
    end

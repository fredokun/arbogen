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

open Frontend
open Options
module WeightedGrammar = Boltzmann.WeightedGrammar

let version_str = "arbogen v1.0c"
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

   ("-eps2", Arg.Float (fun f -> ParseUtil.set_option "eps2" (Vfloat f)),
    "<x> : set the epsilon for simple iteration (a positive float number)");

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

let print_tree tree =
  (* XXX. ugly workaround *)
  let tree = Tree.annotate tree in

  let {with_type; with_id; indent; _} = global_options in
  let arb_printer = Tree.output_arb ~show_type:with_type ~show_id:with_id ~indent in
  let dot_printer = Tree.output_dot ~show_type:with_type ~show_id:with_id ~indent in
  let xml_printer = Tree.output_xml ~show_type:with_type ~show_id:with_id ~indent in

  let print printer filename typ =
    if filename = "" then printer stdout tree
    else begin
      Format.printf "Saving file to '%s%s'@." filename typ;
      let out = open_out (filename ^ typ) in
      printer out tree;
      close_out out
    end
  in

  match global_options.output_type with
  | 0 -> print arb_printer global_options.fileName ".arb"
  | 1 -> print dot_printer global_options.fileName ".dot"
  | 2 -> print xml_printer global_options.fileName ".xml"
  | 3 ->
    let filename = if global_options.fileName = "" then "tree" else global_options.fileName in
    print arb_printer filename ".arb";
    print dot_printer filename ".dot";
    print xml_printer filename ".xml"
  |_ -> failwith "unreachable case"

let parse_grammar () =
  let opts, g = ParseUtil.parse_from_file global_options.grammar_file in
  ParseUtil.set_options ~preserve:true opts;
  g

let make_oracle grammar =
  let open Oracles.Naive in
  let oracle_config = {
      epsilon1 = global_options.epsilon1;
      epsilon2 = global_options.epsilon2;
      zmin = 0.;
      zmax = 1.;
      zstart = global_options.zstart;
    } in
  make oracle_config grammar

let get_rng : string -> (module Randtools.Sig.S) = function
  | "ocaml" -> (module Randtools.OcamlRandom)
  | "randu" -> (module Randtools.Randu)
  | "randnull" -> (module Randtools.Randnull)
  | name -> Format.kasprintf invalid_arg "Unknown PRNG: %s" name

let init_rng () =
  let module Rand = (val get_rng global_options.randgen) in
  let seed = match global_options.random_seed with
    | Some seed -> seed
    | None -> Rand.self_init (); Rand.int 274537
  in
  if global_options.verbosity >= 2 then
    Format.printf "[SEED] starting seed = %d@." seed;
  Rand.init seed;
  (module Rand: Randtools.Sig.S)

let () =
  Arg.parse speclist
    (fun arg ->
       if global_options.grammar_file = ""
       then global_options.grammar_file <- arg
       else (Format.eprintf "Error: grammar file already set, argument '%s' rejected@." arg ; exit 1))
    usage;
  ParseUtil.extra_checks ();

  if (global_options.verbosity) > 0 then Format.printf "%s@." banner;

  if global_options.print_oracle then begin
    let grammar = parse_grammar () in
    Format.printf "[GRAMMAR]: grammar parsed is :\n%a@." Grammar.pp grammar;
    let oracle = make_oracle grammar in
    Format.printf "[ORACLE]: found singularity at z=%F@." oracle.z;
    let wgrm = WeightedGrammar.of_grammar oracle grammar in
    Format.printf "[ORACLE]: weighted grammar is :@\n%a@." WeightedGrammar.pp wgrm;
    exit 0
  end;

  let result =
    if not global_options.with_state then begin
      let grammar = parse_grammar () in
      let oracle = make_oracle grammar in
      let rng = init_rng () in
      if (global_options.verbosity) > 0 then Format.printf "Generating tree...@.";
      Boltzmann.Gen.generator
        grammar
        oracle
        rng
        ~size_min:global_options.size_min
        ~size_max:global_options.size_max
        ~max_try:global_options.max_try
    end else begin
      if (global_options.verbosity) > 0 then
        Format.printf "Loading state file: %s@." global_options.state_file;

      let state = Boltzmann.GenState.from_file global_options.state_file in
      global_options.randgen <- state.randgen;
      let module Rand = (val get_rng state.randgen) in
      Rand.(State.from_bytes state.rnd_state |> set_state);

      let tree, size = Boltzmann.Gen.gen (module Rand) state.weighted_grammar in
      Some (tree, size, state)
    end
  in
  match result with
  | None ->
    Format.eprintf "No tree generated ==> try to use different parameters@.";
    exit 1
  | Some (tree, size, state) ->
    if global_options.verbosity > 0 then
      Format.printf "==> Tree generated with %d nodes@." size;

    let out_state =
      if global_options.fileName = "" then "state"
      else global_options.fileName ^ ".state"
    in
    if global_options.verbosity >= 2 then
      Format.printf "==> Saving state to file '%s'@." out_state;

    Boltzmann.GenState.to_file out_state state;
    print_tree tree

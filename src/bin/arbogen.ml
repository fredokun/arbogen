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

module WeightedGrammar = Boltzmann.WeightedGrammar
module Options = Frontend.Options

let version_str = "arbogen v1.0c"

let usage = "Usage: arbogen <opt> <specfile>.spec"

let banner =
  "\n\
  \              A      ...:'....:'':...':......\n\
  \              R    :''   ._   .  `.  \\   ,   '':\n\
  \              B    ':  .   \" .|    \\  `>/   _.-':\n\
  \              O   .:'  .`'.   `-.  '. /'  ,..  .:\n\
  \              G  :'        `.    `\\| \\./   ' :\n\
  \              E  :. ,,-'''''  \"-.   |   | ....:\n\
  \              N   '.      ..'''  `\\ :   |\n\
  \                    ''''''''       \\'   |\n\
  \           *fast* uniform random    |  =|\n\
  \                  tree generator    |   |\n\
  \                                    |-  |\n\
  \              '''''''''''''''''''''''''''''''''''''''\n\
  \              (C) F. Peschanski et al. under the GPL\n"

let speclist =
  let set_verbosity n =
    if n < 0 then (
      Format.eprintf "Error: wrong verbosity level %d => must be positive@." n;
      exit 1 )
    else Options.globals.verbosity <- n
  in
  [ ( "-version"
    , Arg.Unit
        (fun () ->
          Format.printf "%s@." version_str;
          exit 0 )
    , "print version information" )
  ; ( "-verbose"
    , Arg.Int set_verbosity
    , "<n> : set the verbosity level to <n>  (a non-negative positive integer)"
    )
  ; ("-v", Arg.Int set_verbosity, "<n> : same as -verbose <n>")
  ; ( "-min"
    , Arg.Int (fun n -> Options.set "min" (Int n))
    , "<n> : set the minimum size for the generated tree to <n> (a \
       non-negative integer)" )
  ; ( "-max"
    , Arg.Int (fun n -> Options.set "max" (Int n))
    , "<n> : set the maximum size for the generated tree to <n> (a \
       non-negative integer)" )
  ; ( "-seed"
    , Arg.Int (fun n -> Options.set "seed" (Int n))
    , "<n> : set the random generator seed to <n>" )
  ; ( "-eps1"
    , Arg.Float (fun f -> Options.set "eps1" (Float f))
    , "<x> : set the epsilon for singularity search (a positive float number)"
    )
  ; ( "-eps2"
    , Arg.Float (fun f -> Options.set "eps2" (Float f))
    , "<x> : set the epsilon for simple iteration (a positive float number)" )
  ; ( "-try"
    , Arg.Int (fun n -> Options.set "try" (Int n))
    , "<n> : set the maximum of tries when generating trees" )
  ; ( "-otype"
    , Arg.String
        (function
        | "arb" ->
          Options.globals.output_type <- 0
        | "dot" ->
          Options.globals.output_type <- 1
        | "xml" ->
          Options.globals.output_type <- 2
        | "all" ->
          Options.globals.output_type <- 3
        | _ ->
          Format.eprintf "Error: otype must be in [arb|dot|xml|all]@.";
          exit 1 )
    , "<n>: set the type [arb|dot|xml|all] of the generated tree" )
  ; ( "-o"
    , Arg.String (fun x -> Options.globals.fileName <- x)
    , "<x>: set the name of the file to be created at end of execution" )
  ; ( "-zstart"
    , Arg.Float (fun f -> Options.set "zstart" (Float f))
    , "<x>: sets the value of zstart" )
  ; ( "-state"
    , Arg.String
        (fun x ->
          Options.globals.state_file <- x;
          Options.globals.with_state <- true )
    , "<n>: set the name of state file" )
  ; ( "-id"
    , Arg.Unit (fun () -> Options.globals.with_id <- true)
    , ": number the nodes" )
  ; ( "-typ"
    , Arg.Unit (fun () -> Options.globals.with_type <- true)
    , ": show the type of nodes" )
  ; ( "-randgen"
    , Arg.String (fun s -> Options.set "randgen" (String s))
    , "[ocaml|randu|randnull] : set the random number generator" )
  ; ( "-oracle"
    , Arg.Unit (fun () -> Options.globals.print_oracle <- true)
    , ": output an oracle" )
  ; ( "-indent"
    , Arg.Unit (fun () -> Options.globals.indent <- true)
    , ": indent the output" ) ]

let print_tree tree =
  (* XXX. ugly workaround *)
  let tree = Tree.annotate tree in
  let Options.{with_type; with_id; indent; _} = Options.globals in
  let arb_printer =
    Tree.output_arb ~show_type:with_type ~show_id:with_id ~indent
  in
  let dot_printer =
    Tree.output_dot ~show_type:with_type ~show_id:with_id ~indent
  in
  let xml_printer =
    Tree.output_xml ~show_type:with_type ~show_id:with_id ~indent
  in
  let print printer filename typ =
    if filename = "" then printer stdout tree
    else (
      Format.printf "Saving file to '%s%s'@." filename typ;
      let out = open_out (filename ^ typ) in
      printer out tree; close_out out )
  in
  match Options.globals.output_type with
  | 0 ->
    print arb_printer Options.globals.fileName ".arb"
  | 1 ->
    print dot_printer Options.globals.fileName ".dot"
  | 2 ->
    print xml_printer Options.globals.fileName ".xml"
  | 3 ->
    let filename =
      if Options.globals.fileName = "" then "tree" else Options.globals.fileName
    in
    print arb_printer filename ".arb";
    print dot_printer filename ".dot";
    print xml_printer filename ".xml"
  | _ ->
    failwith "unreachable case"

let parse_grammar () =
  let opts, g = Frontend.parse_from_file Options.globals.grammar_file in
  Options.set_all ~preserve:true opts;
  g

let make_oracle grammar =
  let open Oracles.Naive in
  let oracle_config =
    { epsilon1= Options.(WithDefault.value globals.epsilon1)
    ; epsilon2= Options.(WithDefault.value globals.epsilon2)
    ; zstart= Options.(WithDefault.value globals.zstart) }
  in
  make ~config:oracle_config grammar

let get_rng : string -> (module Randtools.S) = function
  | "ocaml" ->
    (module Randtools.OcamlRandom)
  | "randu" ->
    (module Randtools.Randu)
  | "randnull" ->
    (module Randtools.Randnull)
  | name ->
    Format.kasprintf invalid_arg "Unknown PRNG: %s" name

let init_rng () =
  let module Rand = (val get_rng Options.globals.randgen) in
  let seed =
    match Options.globals.random_seed with
    | Some seed ->
      seed
    | None ->
      Rand.self_init (); Rand.int 274537
  in
  if Options.globals.verbosity >= 2 then
    Format.printf "[SEED] starting seed = %d@." seed;
  Rand.init seed;
  (module Rand : Randtools.S)

let () =
  Arg.parse speclist
    (fun arg ->
      if Options.globals.grammar_file = "" then
        Options.globals.grammar_file <- arg
      else (
        Format.eprintf
          "Error: grammar file already set, argument '%s' rejected@." arg;
        exit 1 ) )
    usage;
  Options.extra_checks ();
  if Options.globals.verbosity > 0 then Format.printf "%s@." banner;
  if Options.globals.print_oracle then (
    let grammar = parse_grammar () in
    Format.printf "[GRAMMAR]: grammar parsed is :\n%a@." Grammar.pp grammar;
    let oracle = make_oracle grammar in
    Format.printf "[ORACLE]: found singularity at z=%F@." oracle.z;
    let wgrm = WeightedGrammar.of_grammar oracle grammar in
    Format.printf "[ORACLE]: weighted grammar is :@\n%a@." WeightedGrammar.pp
      wgrm;
    exit 0 );
  let state =
    if Options.globals.with_state then (
      if Options.globals.verbosity > 0 then
        Format.printf "Loading state file: %s@." Options.globals.state_file;
      let state = Boltzmann.GenState.from_file Options.globals.state_file in
      Options.globals.randgen <- state.randgen;
      Some state )
    else None
  in
  let module Rng =
  (val match state with Some s -> get_rng s.randgen | None -> init_rng ())
  in
  let result, wgrm =
    match state with
    | None ->
      let grammar = parse_grammar () in
      let oracle = make_oracle grammar in
      if Options.globals.verbosity > 0 then Format.printf "Generating tree...@.";
      let tree =
        Boltzmann.Gen.generator grammar oracle
          (module Rng)
          ~size_min:Options.(WithDefault.value globals.size_min)
          ~size_max:Options.(WithDefault.value globals.size_max)
          ~max_try:Options.(WithDefault.value globals.max_try)
      in
      (tree, WeightedGrammar.of_grammar oracle grammar)
    | Some state ->
      Rng.(State.from_bytes state.rnd_state |> set_state);
      let tree = Boltzmann.Gen.free_gen (module Rng) state.weighted_grammar in
      (Some tree, state.weighted_grammar)
  in
  match result with
  | None ->
    Format.eprintf "No tree generated ==> try to use different parameters@.";
    exit 1
  | Some (tree, size) ->
    let final_state =
      Boltzmann.GenState.
        { randgen= Rng.name
        ; rnd_state= Rng.(State.to_bytes (get_state ()))
        ; weighted_grammar= wgrm }
    in
    if Options.globals.verbosity > 0 then
      Format.printf "==> Tree generated with %d nodes@." size;
    let out_state =
      if Options.globals.fileName = "" then "state"
      else Options.globals.fileName ^ ".state"
    in
    if Options.globals.verbosity >= 2 then
      Format.printf "==> Saving state to file '%s'@." out_state;
    Boltzmann.GenState.to_file out_state final_state;
    print_tree tree

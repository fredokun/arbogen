(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: ParseUtil                                     *
 * -------                                               *
 * Options Parser                                        *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *	     under the                                       *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Options

let parse_from_channel chan =
  let lexbuf = Lexing.from_channel chan in
  let params, parse_tree = Parser.start Lexer.token lexbuf in
  close_in chan;
  params, ParseTree.to_grammar parse_tree

let parse_from_file filename =
  let chan = open_in filename in
  parse_from_channel chan

let fail format = Format.kasprintf failwith format

let as_int opt_name = function
  | Vint n -> n
  | _ -> fail "type error: %s expects an integer" opt_name

let as_float opt_name = function
  | Vfloat f -> f
  | _ -> fail "type error: %s expects a float" opt_name

let as_string opt_name = function
  | Vstring s -> s
  | _ -> fail "type error: %s expects a string" opt_name

let set_option ?(preserve=false) name value =
  match name with
  | "min" ->
    let value = as_int name value in
    if value < 0 then fail "min must be non-negative";
    if not global_options.size_min_set || not preserve then begin
      global_options.size_min_set <- true;
      global_options.size_min <- value
    end

  | "max" ->
    let value = as_int name value in
    if value < 0 then fail "max must be non-negative";
    if not global_options.size_max_set || not preserve then
      begin
        global_options.size_max_set <- true;
        global_options.size_max <- value
      end

  | "seed" ->
    begin match global_options.random_seed with
      | Some _ when preserve -> ()
      | _ -> global_options.random_seed <- Some (as_int "seed" value)
    end

  | "eps1" ->
    let value = as_float name value in
    if value <= 0. then fail "eps1 must be positive";
    if not global_options.epsilon1_set || not preserve then begin
      global_options.epsilon1_set <- true;
      global_options.epsilon1 <- value
    end

  | "eps1_factor" ->
    let value = as_float name value in
    if value <= 0. then fail "eps1_factor must be positive";
    if not global_options.epsilon1_factor_set || not preserve then begin
      global_options.epsilon1_factor_set <- true;
      global_options.epsilon1_factor <- value
    end

  | "eps2" ->
    let value = as_float name value in
    if value <= 0. then fail "eps2 must be positive";
    if not global_options.epsilon2_set || not preserve then begin
      global_options.epsilon2_set <- true;
      global_options.epsilon2 <- value
    end

  | "eps2_factor" ->
    let value = as_float name value in
    if value <= 0. then fail "eps2_factor must be positive";
    if not global_options.epsilon2_factor_set || not preserve then begin
      global_options.epsilon2_factor_set <- true;
      global_options.epsilon2_factor <- value
    end

  | "reject_ratio" ->
    let value = as_float name value in
    if value < 0. then fail "reject_ratio must be non-negative";
    if not global_options.ratio_rejected_set || not preserve then begin
      global_options.ratio_rejected_set <- true;
      global_options.ratio_rejected <- value
    end

  | "max_refine" ->
    let value = as_int name value in
    if value <= 0 then fail "max_refine must be positive";
    if not global_options.max_refine_set || not preserve then begin
      global_options.max_refine_set <- true;
      global_options.max_refine <- value
    end

  | "try" ->
    let value = as_int name value in
    if value <= 0 then fail "try must be positive";
    if not global_options.max_try_set || not preserve then begin
      global_options.max_try_set <- true;
      global_options.max_try <- value
    end

  | "zstart" ->
    let value = as_float name value in
    if value < 0. || value > 1. then fail "zstart must be between 0 and 1";
    if not global_options.zstart_set || not preserve then begin
      global_options.zstart_set <- true;
      global_options.zstart <- value
    end

  | "randgen" ->
    let value = as_string name value in
    let valid_names = List.map fst RandGen.all_rand_gens in
    if not (List.mem value valid_names) then
      fail "rangen must belong to: %s" (String.concat "|" valid_names);
    global_options.randgen <- value

  | _ -> fail "Unknown parameter: %s" name

let set_options ?(preserve=false) parameters =
  List.iter
    (function Param (name, value) -> set_option ~preserve name value)
    parameters

(** Checks that are not bound to a single parameters *)
let extra_checks () =
  if global_options.size_min > global_options.size_max then
    fail "size_min must be smaller than size_max"

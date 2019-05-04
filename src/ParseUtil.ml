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
  let res = Parser.start Lexer.token lexbuf in
  close_in chan;
  res

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
    if not global_options.size_min_set || not preserve then begin
      global_options.size_min_set <- true;
      global_options.size_min <- as_int name value
    end

  | "max" ->
    if not global_options.size_max_set || not preserve then
      begin
        global_options.size_max_set <- true;
        global_options.size_max <- as_int name value
      end

  | "seed" ->
    begin match global_options.random_seed with
      | Some _ when preserve -> ()
      | _ -> global_options.random_seed <- Some (as_int "seed" value)
    end

  | "eps1" ->
    if not global_options.epsilon1_set || not preserve then begin
      global_options.epsilon1_set <- true;
      global_options.epsilon1 <- as_float name value
    end

  | "eps1_factor" ->
    if not global_options.epsilon1_factor_set || not preserve then begin
      global_options.epsilon1_factor_set <- true;
      global_options.epsilon1_factor <- as_float name value
    end

  | "eps2" ->
    if not global_options.epsilon2_set || not preserve then begin
      global_options.epsilon2_set <- true;
      global_options.epsilon2 <- as_float name value
    end

  | "eps2_factor" ->
    if not global_options.epsilon2_factor_set || not preserve then begin
      global_options.epsilon2_factor_set <- true;
      global_options.epsilon2_factor <- as_float name value
    end

  | "reject_ratio" ->
    if not global_options.ratio_rejected_set || not preserve then begin
      global_options.ratio_rejected_set <- true;
      global_options.ratio_rejected <- as_float name value
    end

  | "max_refine" ->
    if not global_options.max_refine_set || not preserve then begin
      global_options.max_refine_set <- true;
      global_options.max_refine <- as_int name value
    end

  | "try" ->
    if not global_options.max_try_set || not preserve then begin
      global_options.max_try_set <- true;
      global_options.max_try <- as_int name value
    end

  | "zstart" ->
    if not global_options.zstart_set || not preserve then begin
      global_options.zstart_set <- true;
      global_options.zstart <- as_float name value
    end

  | "randgen" -> global_options.randgen <- as_string name value

  | _ -> fail "Unknown parameter: %s" name

let set_options ?(preserve=false) parameters =
  List.iter
    (function Param (name, value) -> set_option ~preserve name value)
    parameters

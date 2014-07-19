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
 *	     under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Options
open Ast

let parse_from_channel chan =
  let lexbuf = Lexing.from_channel chan in
  let res = Parser.start Lexer.token lexbuf in
  close_in chan;
  res  

let parse_from_file filename = 
  let chan = open_in filename in
  parse_from_channel chan

let rec set_options options =
  match options with
    | [] -> ()
    | (Ast.Param (name, value)) :: q ->
      begin
	match name with
	  | "min" -> 
            begin
              global_options.size_min_set <- true;
              global_options.size_min <-  
		(match value with
		  | Vint n -> n
		  | _ -> failwith "type error")
            end
	  | "max" ->
            begin
              global_options.size_max_set <- true;
              global_options.size_max <-  
		(match value with
		  | Vint n -> n
		  | _ -> failwith "type error")
            end
	  | "seed" ->
            begin
              global_options.self_seed_set <- true;
              global_options.self_seed <- false;
              global_options.random_seed_set <- true;
              global_options.random_seed <-
		(match value with
		  | Vint n -> n
		  | _ -> failwith "type error")
            end
	  | "eps1" ->
            begin
              global_options.epsilon1_set <- true;
              global_options.epsilon1 <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | "eps1_factor" ->
            begin
              global_options.epsilon1_factor_set <- true;
              global_options.epsilon1_factor <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | "eps2" ->
            begin
              global_options.epsilon2_set <- true;
              global_options.epsilon2 <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | "eps2_factor" ->
	    begin
              global_options.epsilon2_factor_set <- true;
              global_options.epsilon2_factor <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | "reject_ratio" ->
            begin
              global_options.ratio_rejected_set <- true;
              global_options.ratio_rejected <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | "max_refine" ->
            begin
              global_options.max_refine_set <- true;
              global_options.max_refine <-  
		(match value with
		  | Vint n -> n
		  | _ -> failwith "type error")
            end
	  | "try" ->
            begin
              global_options.max_try_set <- true;
              global_options.max_try <-  
		(match value with
		  | Vint n -> n
		  | _ -> failwith "type error")
            end
	  | "zstart" ->
            begin
              global_options.zstart_set <- true;
              global_options.zstart <-  
		(match value with
		  | Vfloat n -> n
		  | _ -> failwith "type error")
            end
	  | _ -> failwith "Unknown parameter"
      end;
      set_options q


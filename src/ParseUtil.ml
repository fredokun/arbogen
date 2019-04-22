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

let rec set_options options =
  match options with
  | [] -> ()
  | (Param (name, value)) :: q ->
    begin
	    match name with
	    | "min" ->
        begin
          if not global_options.size_min_set then
            begin
              global_options.size_min_set <- true;
              global_options.size_min <-
		            (match value with
		            | Vint n -> n
		            | _ -> failwith "type error")
            end
        end
	    | "max" ->
        begin
          if not global_options.size_max_set then
            begin
              global_options.size_max_set <- true;
              global_options.size_max <-
		            (match value with
		            | Vint n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "seed" ->
        begin
          if not global_options.self_seed_set then
            begin
              global_options.self_seed_set <- true;
              global_options.self_seed <- false;
              global_options.random_seed_set <- true;
              global_options.random_seed <-
		            (match value with
		            | Vint n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "eps1" ->
        begin
          if not global_options.epsilon1_set then
            begin
              global_options.epsilon1_set <- true;
              global_options.epsilon1 <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "eps1_factor" ->
        begin
          if not global_options.epsilon1_factor_set then
            begin
              global_options.epsilon1_factor_set <- true;
              global_options.epsilon1_factor <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "eps2" ->
        begin
          if not global_options.epsilon2_set then

            begin
              global_options.epsilon2_set <- true;
              global_options.epsilon2 <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "eps2_factor" ->
	      begin
          if not global_options.epsilon2_factor_set then
            begin
              global_options.epsilon2_factor_set <- true;
              global_options.epsilon2_factor <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "reject_ratio" ->
        begin
          if not global_options.ratio_rejected_set then
            begin
              global_options.ratio_rejected_set <- true;
              global_options.ratio_rejected <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "max_refine" ->
        begin
          if not global_options.max_refine_set then
            begin
              global_options.max_refine_set <- true;
              global_options.max_refine <-
		            (match value with
		            | Vint n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "try" ->
        begin
          if not global_options.max_try_set then
            begin
              global_options.max_try_set <- true;
              global_options.max_try <-
		            (match value with
		            | Vint n -> n
		            | _ -> failwith "type error")
            end
        end

	    | "zstart" ->
        begin
          if not global_options.zstart_set then
            begin
              global_options.zstart_set <- true;
              global_options.zstart <-
		            (match value with
		            | Vfloat n -> n
		            | _ -> failwith "type error")
            end
        end
	    | "randgen" ->
        begin
          global_options.randgen <-
		      (match value with
		      | Vstring s -> s
		      | _ -> failwith "type error")
        end
	    | _ -> failwith "Unknown parameter"
    end;
    set_options q

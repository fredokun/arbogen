let fail format = Format.kasprintf failwith format


(** Values taken by configuration options *)
module Value = struct
  type t =
    | Int of int
    | Float of float
    | String of string

  (** {3 Conversion helpers} *)

  let as_int opt_name = function
    | Int n -> n
    | _ -> fail "type error: %s expects an integer" opt_name

  let as_float opt_name = function
    | Float f -> f
    | _ -> fail "type error: %s expects a float" opt_name

  let as_string opt_name = function
    | String s -> s
    | _ -> fail "type error: %s expects a string" opt_name
end


(** A configuration option is given by a name and a value *)
type parameter = string * Value.t


(** A record holding all the configuration options *)
type t = {
  mutable grammar_file: string;
  mutable verbosity: int;
  mutable random_seed: int option;
  mutable size_min: int;
  mutable size_min_set: bool;
  mutable size_max: int;
  mutable size_max_set: bool;
  mutable epsilon1: float;
  mutable epsilon1_set: bool;
  mutable epsilon2: float;
  mutable epsilon2_set: bool;
  mutable with_id : bool;
  mutable with_type : bool;
  mutable max_try: int;
  mutable max_try_set: bool;
  mutable output_type: int;
  mutable fileName: string;
  mutable zstart: float;
  mutable zstart_set: bool;
  mutable with_state:bool;
  mutable state_file:string;
  mutable randgen:string;
  mutable indent:bool;
  mutable print_oracle:bool
}

(** Global variable holding the current configuration *)
let globals = {
  grammar_file = "";
  verbosity = 1;
  random_seed = None;
  size_min = 10;
  size_min_set = false;
  size_max = 20;
  size_max_set = false;
  epsilon1 = 0.001;
  epsilon1_set = false;
  epsilon2 = 0.0001;
  epsilon2_set = false;
  with_id = false;
  with_type = false;
  max_try = 100;
  max_try_set = false;
  output_type = 0;
  fileName = "";
  zstart = 0.0;
  zstart_set = false;
  with_state = false;
  state_file = "";
  randgen = "ocaml";
  indent=false;
  print_oracle=false;
}

(** Helper function to sanitise config options and set them.
    The [preserve] flag tells whether an option that had previously been set
    should be preserved or erased by the new option. *)
let set ?(preserve=false) name value =
  match name with
  | "min" ->
    let value = Value.as_int name value in
    if value < 0 then fail "min must be non-negative";
    if not globals.size_min_set || not preserve then begin
      globals.size_min_set <- true;
      globals.size_min <- value
    end

  | "max" ->
    let value = Value.as_int name value in
    if value < 0 then fail "max must be non-negative";
    if not globals.size_max_set || not preserve then
      begin
        globals.size_max_set <- true;
        globals.size_max <- value
      end

  | "seed" ->
    begin match globals.random_seed with
      | Some _ when preserve -> ()
      | _ -> globals.random_seed <- Some (Value.as_int "seed" value)
    end

  | "eps1" ->
    let value = Value.as_float name value in
    if value <= 0. then fail "eps1 must be positive";
    if not globals.epsilon1_set || not preserve then begin
      globals.epsilon1_set <- true;
      globals.epsilon1 <- value
    end

  | "eps2" ->
    let value = Value.as_float name value in
    if value <= 0. then fail "eps2 must be positive";
    if not globals.epsilon2_set || not preserve then begin
      globals.epsilon2_set <- true;
      globals.epsilon2 <- value
    end

  | "try" ->
    let value = Value.as_int name value in
    if value <= 0 then fail "try must be positive";
    if not globals.max_try_set || not preserve then begin
      globals.max_try_set <- true;
      globals.max_try <- value
    end

  | "zstart" ->
    let value = Value.as_float name value in
    if value < 0. || value > 1. then fail "zstart must be between 0 and 1";
    if not globals.zstart_set || not preserve then begin
      globals.zstart_set <- true;
      globals.zstart <- value
    end

  | "randgen" ->
    let value = Value.as_string name value in
    let valid_names = ["ocaml"; "randu"; "randnull"] in
    if not (List.mem value valid_names) then
      fail "rangen must belong to: %s" (String.concat "|" valid_names);
    globals.randgen <- value

  | _ -> fail "Unknown parameter: %s" name

(** Same as {!set} but takes a list of parameters *)
let set_all ?(preserve=false) parameters =
  List.iter
    (fun (name, value) -> set ~preserve name value)
    parameters

(** Perform sanity checks involving several config values *)
let extra_checks () =
  if globals.size_min > globals.size_max then
    fail "size_min must be smaller than size_max"

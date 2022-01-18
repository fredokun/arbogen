let fail format = Format.kasprintf failwith format

(** Values taken by configuration options *)
module Value = struct
  type t = Int of int | Float of float | String of string

  (** {3 Conversion helpers} *)

  let as_int opt_name = function
    | Int n ->
      n
    | _ ->
      fail "type error: %s expects an integer" opt_name

  let as_float opt_name = function
    | Float f ->
      f
    | _ ->
      fail "type error: %s expects a float" opt_name

  let as_string opt_name = function
    | String s ->
      s
    | _ ->
      fail "type error: %s expects a string" opt_name
end

(** A configuration option is given by a name and a value *)
type parameter = string * Value.t

module WithDefault = struct
  (** Either a default value or a user-defined value *)
  type 'a t = Default of 'a | Value of 'a

  (** Get the value hold by a {!t} *)
  let value = function Default x | Value x -> x
end

(** A record holding all the configuration options *)
type t =
  { mutable grammar_file: string
  ; mutable verbosity: int
  ; mutable random_seed: int option
  ; mutable size_min: int WithDefault.t
  ; mutable size_max: int WithDefault.t
  ; mutable epsilon1: float WithDefault.t
  ; mutable epsilon2: float WithDefault.t
  ; mutable with_id: bool
  ; mutable with_type: bool
  ; mutable max_try: int WithDefault.t
  ; mutable output_type: int
  ; mutable fileName: string
  ; mutable zstart: float WithDefault.t
  ; mutable with_state: bool
  ; mutable state_file: string
  ; mutable randgen: string
  ; mutable indent: bool
  ; mutable print_oracle: bool }

(** Global variable holding the current configuration *)
let globals =
  { grammar_file= ""
  ; verbosity= 1
  ; random_seed= None
  ; size_min= Default 10
  ; size_max= Default 20
  ; epsilon1= Default Oracles.Naive.default_config.epsilon1
  ; epsilon2= Default Oracles.Naive.default_config.epsilon2
  ; with_id= false
  ; with_type= false
  ; max_try= Default 100
  ; output_type= 0
  ; fileName= ""
  ; zstart= Default 0.0
  ; with_state= false
  ; state_file= ""
  ; randgen= "ocaml"
  ; indent= false
  ; print_oracle= false }

(** Helper function to sanitise config options and set them. The [preserve] flag
    tells whether an option that had previously been set should be preserved or
    erased by the new option. *)
let set ?(preserve = false) name value =
  match name with
  | "min" -> (
    let value = Value.as_int name value in
    if value < 0 then fail "min must be non-negative";
    match globals.size_min with
    | Default _ ->
      globals.size_min <- Value value
    | Value _ ->
      if not preserve then globals.size_min <- Value value )
  | "max" -> (
    let value = Value.as_int name value in
    if value < 0 then fail "max must be non-negative";
    match globals.size_max with
    | Default _ ->
      globals.size_max <- Value value
    | Value _ ->
      if not preserve then globals.size_max <- Value value )
  | "seed" -> (
    match globals.random_seed with
    | Some _ when preserve ->
      ()
    | _ ->
      globals.random_seed <- Some (Value.as_int "seed" value) )
  | "eps1" -> (
    let value = Value.as_float name value in
    if value <= 0. then fail "eps1 must be positive";
    match globals.epsilon1 with
    | Default _ ->
      globals.epsilon1 <- Value value
    | Value _ ->
      if not preserve then globals.epsilon1 <- Value value )
  | "eps2" -> (
    let value = Value.as_float name value in
    if value <= 0. then fail "eps2 must be positive";
    match globals.epsilon2 with
    | Default _ ->
      globals.epsilon2 <- Value value
    | Value _ ->
      if not preserve then globals.epsilon2 <- Value value )
  | "try" -> (
    let value = Value.as_int name value in
    if value <= 0 then fail "try must be positive";
    match globals.max_try with
    | Default _ ->
      globals.max_try <- Value value
    | Value _ ->
      if not preserve then globals.max_try <- Value value )
  | "zstart" -> (
    let value = Value.as_float name value in
    if value < 0. || value > 1. then fail "zstart must be between 0 and 1";
    match globals.zstart with
    | Default _ ->
      globals.zstart <- Value value
    | Value _ ->
      if not preserve then globals.zstart <- Value value )
  | "randgen" ->
    let value = Value.as_string name value in
    let valid_names = ["ocaml"; "randu"; "randnull"] in
    if not (List.mem value valid_names) then
      fail "rangen must belong to: %s" (String.concat "|" valid_names);
    globals.randgen <- value
  | _ ->
    fail "Unknown parameter: %s" name

(** Same as {!set} but takes a list of parameters *)
let set_all ?(preserve = false) parameters =
  List.iter (fun (name, value) -> set ~preserve name value) parameters

(** Perform sanity checks involving several config values *)
let extra_checks () =
  if globals.size_min > globals.size_max then
    fail "size_min must be smaller than size_max"

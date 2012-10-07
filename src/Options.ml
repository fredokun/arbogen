
type options_record = {
  mutable interactive_mode : bool;
  mutable grammar_file: string;
  mutable verbosity: int;
  mutable self_seed : bool;
  mutable random_seed: int;
  mutable size_min: int;
  mutable size_max: int;
  mutable epsilon1: float;
  mutable epsilon1_factor: float;
  mutable epsilon2: float;
  mutable epsilon2_factor: float;
  mutable with_prefix: bool;
  mutable idprefix: string;
  mutable max_try: int;
  mutable ratio_rejected: float;
  mutable max_refine: int;
  mutable zstart: float;
} ;;

let global_options = {
  interactive_mode = false;
  grammar_file = "";
  verbosity = 1;
  self_seed = true;
  random_seed = 1234;
  size_min = 10;
  size_max = 100;
  epsilon1 = 0.001;
  epsilon1_factor = 0.1;
  epsilon2 = 0.0001;
  epsilon2_factor = 0.1;
  with_prefix = false;
  idprefix =  "";
  max_try = 100;
  ratio_rejected = 0.8;
  max_refine = 6;
  zstart = 0.1;
} ;;

exception Option_Error of string ;;


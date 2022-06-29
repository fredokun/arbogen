(** Singularity search and approximation of generating functions *)

(** For the grammars supported by Arbogen, it is enough to know the
    values of the different generating functions at one point [z]. *)
type t = {z: float; values: float array; derivate_values: float array}

let init n z = {z; values= Array.make n 0.; derivate_values= Array.make n 0.}

let copy oracle =
  { z= oracle.z
  ; values= Array.copy oracle.values
  ; derivate_values= Array.copy oracle.derivate_values }

(** {2 Generating function evaluation} *)

(** Evaluation of generating functions of grammar elements based on the values
    provided by an oracle. *)
module Eval = struct
  (** Evaluate an expression. *)
  let expression oracle =
    let rec aux : int Grammar.expression -> float = function
      | Z n ->
        oracle.z ** float_of_int n
      | Product args ->
        List.fold_left (fun p e -> p *. aux e) 1. args
      | Union args ->
        List.fold_left (fun s e -> s +. aux e) 0. args
      | Seq e ->
        1. /. (1. -. aux e)
      | Ref i ->
        oracle.values.(i)
    in
    aux

  let deriv_expression oracle =
    let rec aux : int Grammar.expression -> float = function
      | Z n ->
        if n == 0 then 0.
        else float_of_int n *. (oracle.z ** float_of_int (n - 1))
      | Product args ->
        let factors = List.map (fun e -> (expression oracle e, aux e)) args in
        let product = List.fold_left (fun p (e, _) -> p *. e) 1. factors in
        List.fold_left
          (fun res (e, e') -> res +. (product *. e' /. e))
          0. factors
      | Union args ->
        List.fold_left (fun s e -> s +. aux e) 0. args
      | Seq e ->
        aux e /. ((1. -. expression oracle e) ** 2.)
      | Ref i ->
        oracle.derivate_values.(i)
    in
    aux

  (** Same as [!grammar] but the result of the evaluation is stored in the array
      passed as first argument. *)
  let grammar_inplace oracle_dest oracle (grammar : Grammar.t) =
    Array.iteri
      (fun i r -> oracle_dest.values.(i) <- expression oracle r)
      grammar.rules;
    Array.iteri
      (fun i r -> oracle_dest.derivate_values.(i) <- deriv_expression oracle r)
      grammar.rules

  (** Evaluate each rule of the grammar. *)
  let grammar oracle (grammar : Grammar.t) =
    let dest = init (Array.length grammar.rules) oracle.z in
    grammar_inplace dest oracle grammar;
    dest
end

(** {2 Naive oracle implementation} *)

(** Naive oracle obtained by approximating fix points by simple iteration *)
module Naive = struct
  type value = Val of t | Diverge

  type config =
    {epsilon1: float; epsilon2: float; epsilon3: float; zstart: float}

  (** The distance for the uniform norm *)
  let distance (v1 : float array) (v2 : float array) =
    let dist = ref 0. in
    Array.iter2 (fun x y -> dist := max !dist (abs_float (x -. y))) v1 v2;
    !dist

  let diverge (epsilon2 : float) : float array -> bool =
    (* XXX. dangerous but letting too_big grow too much is dangerous too *)
    let too_big = min (1.0 /. epsilon2) 1000. in
    let is_nan x = x <> x in
    Array.exists (fun x -> x > too_big || x < 0. || is_nan x)

  let iteration_simple grammar init_values epsilon2 =
    (* Format.eprintf "COUCOU !!! @."; *)
    let rec iterate v1 v2 =
      (* Array.iter (Format.eprintf "%f,") v1.values; *)
      (* Format.eprintf "@."; *)
      Eval.grammar_inplace v2 v1 grammar;
      if diverge epsilon2 v2.values then Diverge
      else if distance v1.values v2.values <= epsilon2 then Val v2
      else iterate v2 v1
    in
    (* Only allocate to arrays and swap them at each iteration *)
    let v1 = copy init_values in
    let v2 = copy init_values in
    iterate v1 v2

  let search_singularity {epsilon1; epsilon2; zstart; _} grammar =
    let len = Array.length grammar.Grammar.rules in
    let rec search init_values zmin zmax =
      if zmax -. zmin < epsilon1 then
        ( zmin
        , zmax
        , iteration_simple grammar {init_values with z= zmin} epsilon2 )
      else
        match iteration_simple grammar init_values epsilon2 with
        | Val values ->
          search {values with z= (values.z +. zmax) /. 2.} values.z zmax
        | Diverge ->
          let init_values' = init len ((zmin +. init_values.z) /. 2.) in
          search init_values' zmin init_values.z
    in
    let init_values = init len zstart in
    match search init_values 0. 1. with
    | _, _, Diverge ->
      failwith "search_singularity failed to find the singularity"
    | zmin, zmax, Val v ->
      (zmin, zmax, v)

  (* Find the parameter such that size of `Ref 0` is at `epsilon3 * n` of `n` *)
  let search_expectation {epsilon1; epsilon2; epsilon3; zstart} n grammar =
    let len = Array.length grammar.Grammar.rules in
    let rec search init_values zmin zmax =
      if zmax -. zmin < epsilon1 then
        ( zmin
        , zmax
        , iteration_simple grammar {init_values with z= zmin} epsilon2 )
      else
        let eval = iteration_simple grammar init_values epsilon2 in
        match eval with
        | Diverge ->
          let init_values' = init len ((zmin +. init_values.z) /. 2.) in
          search init_values' zmin init_values.z
        | Val v ->
          let expectation = v.z *. v.derivate_values.(0) /. v.values.(0) in
          let diff = (float_of_int n -. expectation) /. float_of_int n in
          if abs_float diff < epsilon3 then (zmin, zmax, eval)
          else if diff < 0. then
            let init_values' = init len ((zmin +. init_values.z) /. 2.) in
            search init_values' zmin init_values.z
          else
            let init_values' = init len ((zmax +. init_values.z) /. 2.) in
            search init_values' init_values.z zmax
    in
    let init_values = init len zstart in
    match search init_values 0. 1. with
    | _, _, Diverge ->
      failwith "search_singularity failed to find the singularity"
    | zmin, zmax, Val v ->
      (zmin, zmax, v)

  let default_config =
    {epsilon1= 1e-9; epsilon2= 1e-9; zstart= 0.; epsilon3= 0.001}

  let make_singular ?(config = default_config) grammar =
    let _, _, values = search_singularity config grammar in
    values

  let make_expectation ?(config = default_config) n grammar =
    let _, _, values = search_expectation config n grammar in
    values
end

(** Dump an oracle *)
let dump fmt {z; values; derivate_values} =
  let dump_floats fmt floats =
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ')
       (fun fmt -> Format.fprintf fmt "%.18g") )
      fmt (Array.to_list floats)
  in
  Format.fprintf fmt "z: %.18g\n" z;
  Format.fprintf fmt "values: %a\n" dump_floats values;
  Format.fprintf fmt "derivatives: %a" dump_floats derivate_values

(** Dump an oracle to a string *)
let dumps = Format.asprintf "%a" dump

let loads : string -> t =
  let strip_prefix string prefix =
    let prefix_len = String.length prefix in
    let string_len = String.length string in
    if prefix_len > string_len && String.sub string 0 prefix_len <> prefix then
      invalid_arg ("Oracle.loads / " ^ prefix);
    String.sub string prefix_len (string_len - prefix_len)
  in
  let parse_floats s =
    s |> String.split_on_char ' ' |> List.map float_of_string |> Array.of_list
  in
  fun s ->
    let lines = String.split_on_char '\n' s in
    match lines with
    | [z; values; derivatives] | [z; values; derivatives; ""] ->
      { z= float_of_string (strip_prefix z "z: ")
      ; values= parse_floats (strip_prefix values "values: ")
      ; derivate_values= parse_floats (strip_prefix derivatives "derivatives: ")
      }
    | lines ->
      Format.ksprintf invalid_arg "Oracle.loads (found %d lines)"
        (List.length lines)

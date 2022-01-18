(** For the grammars supported by Arbogen, it is enough to know the
    values of the different generating functions at one point [z]. *)
type t = {z: float; values: float array}

(** {2 Generating function evaluation} *)

(** Evaluation of generating functions of grammar elements based on the values
    provided by an oracle. *)
module Eval = struct
  (** Evaluate an expression. *)
  let expression oracle =
    let rec aux : int Grammar.expression -> float = function
      | Z n ->
        oracle.z ** float_of_int n
      | Product (e1, e2) ->
        aux e1 *. aux e2
      | Union (e1, e2) ->
        aux e1 +. aux e2
      | Seq e ->
        1. /. (1. -. aux e)
      | Ref i ->
        oracle.values.(i)
    in
    aux

  (** Same as [!grammar] but the result of the evaluation is stored in the array
      passed as first argument. *)
  let grammar_inplace dest oracle (grammar : Grammar.t) =
    Array.iteri (fun i r -> dest.(i) <- expression oracle r) grammar.rules

  (** Evaluate each rule of the grammar. *)
  let grammar oracle (grammar : Grammar.t) =
    let dest = Array.create_float (Array.length grammar.rules) in
    grammar_inplace dest oracle grammar;
    dest
end

(** {2 Naive oracle implementation} *)

(** Naive oracle obtained by approximating fix points by simple iteration *)
module Naive = struct
  type value = Val of float array | Diverge

  type config = {epsilon1: float; epsilon2: float; zstart: float}

  (** The distance for the uniform norm *)
  let distance v1 v2 =
    let dist = ref 0. in
    Array.iter2 (fun x y -> dist := max !dist (abs_float (x -. y))) v1 v2;
    !dist

  let diverge (epsilon2 : float) : float array -> bool =
    (* XXX. dangerous but letting too_big grow too much is dangerous too *)
    let too_big = min (1.0 /. epsilon2) 1000. in
    let is_nan x = x <> x in
    Array.exists (fun x -> x > too_big || x < 0. || is_nan x)

  let iteration_simple grammar z ?init_values epsilon2 =
    let rec iterate v1 v2 =
      Eval.grammar_inplace v2 {z; values= v1} grammar;
      if diverge epsilon2 v2 then Diverge
      else if distance v1 v2 <= epsilon2 then Val v2
      else iterate v2 v1
    in
    (* Only allocate to arrays and swap them at each iteration *)
    let len = Array.length grammar.Grammar.rules in
    let v1 =
      match init_values with
      | None ->
        Array.make len 0.
      | Some values ->
        Array.copy values
    in
    let v2 = Array.make len 0. in
    iterate v1 v2

  let search_singularity {epsilon1; epsilon2; zstart} grammar =
    let rec search ?init_values zmin zmax zstart =
      if zmax -. zmin < epsilon1 then
        (zmin, zmax, iteration_simple grammar zmin ?init_values epsilon2)
      else
        match iteration_simple grammar zstart epsilon2 with
        | Val values ->
          search ~init_values:values zstart zmax ((zmax +. zstart) /. 2.)
        | Diverge ->
          search zmin zstart ((zmin +. zstart) /. 2.)
    in
    match search 0. 1. zstart with
    | _, _, Diverge ->
      failwith "search_singularity failed to find the singularity"
    | zmin, zmax, Val v ->
      (zmin, zmax, v)

  let default_config = {epsilon1= 1e-9; epsilon2= 1e-9; zstart= 0.}

  let make ?(config = default_config) grammar =
    let z, _, values = search_singularity config grammar in
    {z; values}
end

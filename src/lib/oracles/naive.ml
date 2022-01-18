(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: OracleSimple                                  *
 * -------                                               *
 * The Oracle for the singularity search (naive version) *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

type value =
  | Val of float array
  | Diverge

type config = {
  epsilon1: float;
  epsilon2: float;
  zstart: float;
  zmin: float;
  zmax: float
}

(** The distance for the uniform norm *)
let distance v1 v2 =
  let dist = ref 0. in
  Array.iter2 (fun x y -> dist := max !dist (abs_float (x -. y))) v1 v2;
  !dist

let diverge (epsilon2: float) : float array -> bool =
  (* XXX. dangerous but letting too_big grow too much is dangerous too *)
  let too_big = min (1.0 /. epsilon2) 100. in
  let is_nan x = x <> x in
  Array.exists (fun x -> x > too_big || x < 0. || is_nan x)

let iteration_simple grammar z ?init_values epsilon2 =
  let rec iterate v1 v2 =
    Eval.grammar_inplace v2 {z; values=v1} grammar;
    if diverge epsilon2 v2 then
      Diverge
    else if distance v1 v2 <= epsilon2 then
      Val v2
    else
      iterate v2 v1
  in
  (* Only allocate to arrays and swap them at each iteration *)
  let len = Array.length grammar.Grammar.rules in
  let v1 = match init_values with
    | None -> Array.make len 0.
    | Some values -> Array.copy values
  in
  let v2 = Array.make len 0. in
  iterate v1 v2

let search_singularity {epsilon1; epsilon2; zmin; zmax; zstart} grammar =
  let rec search ?init_values zmin zmax zstart =
    if zmax -. zmin < epsilon1 then
      (zmin, zmax, iteration_simple grammar zmin ?init_values epsilon2)
    else
      match iteration_simple grammar zstart epsilon2 with
      | Val values -> search ~init_values:values zstart zmax ((zmax +. zstart) /. 2.)
      | Diverge -> search zmin zstart ((zmin +. zstart) /. 2.)
  in
  match search zmin zmax zstart with
  | _, _, Diverge ->
    failwith "search_singularity failed to find the singularity"
  | zmin, zmax, Val v ->
    (zmin, zmax, v)

let default_config =
  {epsilon1= 1e-5; epsilon2= 1e-3; zstart= 0.; zmin= 0.; zmax= 1.}

let make ?(config = default_config) grammar =
  let z, _, values = search_singularity config grammar in
  Types.{z; values}

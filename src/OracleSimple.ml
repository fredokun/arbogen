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

open CombSys

type value =
  | Val of float array
  | Diverge

(** The distance for the uniform norm *)
let distance =
  Util.array_fold_left_2 (fun norm y y' -> max norm (abs_float (y -. y'))) 0.0

let diverge (epsilon2: float) : float array -> bool =
  let too_big = 1.0 /. epsilon2 in
  let is_nan x = x <> x in
  Array.exists (fun x -> x > too_big || x < 0. || is_nan x)

(* TODO: allocate only two arrays and swap them at each iteration *)
let iterationSimple (phi: combsys) (z: float) (epsilon2: float) : value =
  let rec iterate y =
    let y' = evaluation phi z y in
    if diverge epsilon2 y' then
      Diverge
    else if distance y y' <= epsilon2 then
      Val y'
    else
      iterate y'
  in
  iterate (Array.make (combsys_size phi) 0.0)

let searchSingularity
    (phi: combsys)
    (zmin: float)
    (zmax: float)
    (epsilon1: float)
    (epsilon2: float)
    (zstart: float)
  : float * float * float array =
  let rec search zmin zmax zstart =
    if zmax -. zmin < epsilon1 then
      (zmin, zmax, iterationSimple phi zmin epsilon2)
    else
      match iterationSimple phi zstart epsilon2 with
      | Val _ -> search zstart zmax ((zmax +. zstart) /. 2.)
      | Diverge -> search zmin zstart ((zmin +. zstart) /. 2.)
  in
  match search zmin zmax zstart with
  | _, _, Diverge -> failwith "searchSingularity failed to find the singularity"
  | zmin, zmax, Val v -> zmin, zmax, v

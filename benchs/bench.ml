let generate ?(seed=42424242) grammar ~size_min ~size_max =
  let oracle_config = Oracles.Naive.{
    epsilon1 = 1e-9; epsilon2 = 1e-9; zstart = 0.; zmin = 0.; zmax = 1.
  } in
  let oracle = Oracles.Naive.make oracle_config grammar in
  let module Rng = Randtools.OcamlRandom in
  Rng.init seed;
  match Boltzmann.Gen.generator
          grammar
          oracle
          (module Rng)
          ~size_min
          ~size_max
          ~max_try:10000
  with
  | Some (tree, size) -> tree, size
  | None -> assert false

let bench ?(size_min=100_000) ?(size_max=200_000) ?(seed=4242424242) grammar =
  ignore (Sys.opaque_identity (generate ~seed grammar ~size_min ~size_max))

let binary () =
  Grammar.{
    names = [|"B"|];
    rules = [|Union (Z 0, Product (Z 1, Product (Reference 0, Reference 0)))|];
  } |> bench

let nary1 () =
  Grammar.{
    names = [|"T"|];
    rules = [|Product (Z 1, Seq (Reference 0))|]
  } |> bench

let nary2 () =
  Grammar.{
    names = [|"T"; "S"|];
    rules = [|
      Product (Z 1, Reference 1);
      Union (Z 0, Product (Reference 0, Reference 1));
    |]
  } |> bench

(* XXX. it takes forever to find a shuffle_plus tree in the window [20, 500000]
   this is really suspicious
   let shuffle_plus () =
   bench ~size_min:20 Grammar.[
      "A", [(0, [Elem "Aplus"]); (0, [Elem "Apar"])];
      "Aplus", [(0, [Elem "Apar"; Elem "Apar"; Seq "Apar"])];
      "Apar", [(1, [Seq "A"])]
    ] *)


let () =
  let res = Benchmark.latencyN 4L [
      "binary", binary, ();
      "nary1", nary1, ();
      "nary2", nary2, ();
      (* "shuffle_plus", shuffle_plus, (); *)
    ] in
  Benchmark.tabulate res

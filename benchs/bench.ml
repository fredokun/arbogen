let generate ?(seed = 42424242) grammar ~size_min ~size_max =
  let oracle = Oracles.Naive.make grammar in
  let module Rng = Randtools.OcamlRandom in
  Rng.init seed;
  match
    Boltzmann.Gen.generator grammar oracle
      (module Rng)
      ~size_min ~size_max ~max_try:10000
  with
  | Some (tree, size) ->
    (tree, size)
  | None ->
    assert false

let bench ?(size_min = 100_000) ?(size_max = 200_000) ?(seed = 4242424242)
    grammar =
  ignore (Sys.opaque_identity (generate ~seed grammar ~size_min ~size_max))

let binary () =
  Grammar.
    { names= [|"B"|]
    ; rules= [|Union (Z 0, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  |> bench

let nary1 () =
  Grammar.{names= [|"T"|]; rules= [|Product (Z 1, Seq (Ref 0))|]} |> bench

let nary2 () =
  Grammar.
    { names= [|"T"; "S"|]
    ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
  |> bench

let shuffle_plus () =
  Grammar.
    { names= [|"A"; "Aplus"; "Apar"|]
    ; rules=
        [| Union (Ref 1, Ref 2)
         ; Product (Ref 2, Product (Ref 2, Seq (Ref 2)))
         ; Product (Z 1, Seq (Ref 0)) |] }
  |> bench

let () =
  let res =
    Benchmark.latencyN 4L
      [ ("binary", binary, ())
      ; ("nary1", nary1, ())
      ; ("nary2", nary2, ())
      ; ("shuffle_plus", shuffle_plus, ()) ]
  in
  Benchmark.tabulate res

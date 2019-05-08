open Arbolib

let generate ?(seed=42424242) grammar ~size_min ~size_max =
  match Gen.generator
          grammar
          ~seed:(Some seed)
          size_min
          size_max
          1e-9    (* epsilon 1 *)
          0.5     (* epsilon_factor 1 *)
          1e-9    (* epsilon 2 *)
          0.5     (* epsilon_factor 2 *)
          10000   (* max_try *)
          0.8     (* ratio_rejected *)
          8       (* max_refine *)
          0.      (* zstart *)
          "ocaml" (* randgen *)
          0       (* verbosity *)
  with
  | Some (tree, size, _) -> tree, size
  | None -> assert false

let bench ?(size_min=100_000) ?(size_max=200_000) ?(seed=4242424242) grammar =
  ignore (Sys.opaque_identity (generate ~seed grammar ~size_min ~size_max))

let binary () =
  bench Grammar.["B", [(0, []); (1, [Elem "B"; Elem "B"])]]

let nary1 () =
  bench Grammar.["T", [(1, [Seq "T"])]]

let nary2 () =
  bench Grammar.[
      "T", [(1, [Elem "S"])];
      "S", [(0, []); (0, [Elem "T"; Elem "S"])]
    ]

(* XXX. it takes forever to find a shuffle_plus tree in the window [20, 500000]
   this is really suspicious
   let shuffle_plus () =
   bench ~size_min:20 Grammar.[
      "A", [(0, [Elem "Aplus"]); (0, [Elem "Apar"])];
      "Aplus", [(0, [Elem "Apar"; Elem "Apar"; Seq "Apar"])];
      "Apar", [(1, [Seq "A"])]
    ] *)

let () =
  let res = Benchmark.latencyN 10L [
      "binary", binary, ();
      "nary1", nary1, ();
      "nary2", nary2, ();
      (* "shuffle_plus", shuffle_plus, (); *)
    ] in
  Benchmark.tabulate res

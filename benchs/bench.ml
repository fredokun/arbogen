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

let simold, simnew =
  Random.init 424242424242;
  let seeds = List.init 5 (fun _ -> Random.bits ()) in
  let size_min = 50_000 in
  let size_max = 200_000 in

  let simold wgrm =
    let counters0 = Gen.init_counters wgrm in
    fun () ->
      Gen.sim 0 counters0 wgrm size_max wgrm.first_rule (module RandGen.OcamlRandom)
  in
  let simnew wgrm =
    let wg = Gen2.WG.of_wgrm wgrm in
    fun () ->
      Gen2.sim (module RandGen.OcamlRandom) size_max wg.Gen2.WG.rules
  in

  let lift sim seed =
    Random.init seed;
    let rec f rej =
      let size = sim () in
      if size < size_min || size > size_max then f (size + rej)
      else rej, size
    in
    f 0
  in

  let wgrm =
    let config = OracleSimple.{
        epsilon1 = 1e-10;
        epsilon2 = 0.001;
        zstart = 0.5;
        zmin = 0.;
        zmax = 1.
      }
    in
    let grammar = Grammar.["T", [(1, [Seq "T"])]] in
    let _, _, wgrm = Gen.compute_weighted_grammar grammar config 0 in
    wgrm
  in

  let print l =
    List.iter (fun (rej, size) -> Format.printf "(%d, %d), " rej size) l;
    Format.printf "\ntotal: %d@." (List.fold_left (fun tot (rej, size) -> tot + rej + size) 0 l)
  in

  let size_old () = List.map (lift (simold wgrm)) seeds |> print in
  let size_new () = List.map (lift (simnew wgrm)) seeds |> print in
  size_old, size_new


let () =
  let res = Benchmark.latencyN 4L [
      (* "binary", binary, ();
      "nary1", nary1, ();
      "nary2", nary2, (); *)
      "simold", simold, ();
      "simnew", simnew, ();
      (* "shuffle_plus", shuffle_plus, (); *)
    ] in
  Benchmark.tabulate res

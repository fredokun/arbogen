open Arbolib


let grammar =
  let pp = Grammar.pp in
  let equal g1 g2 = g1 = g2 in (* XXX *)
  Alcotest.testable pp equal

let parse test_name =
  let _, grammar = ParseUtil.parse_from_file (test_name ^ ".spec") in
  grammar


let binary () =
  let expected = Grammar.{
      rules = [|
        [(1, [Elem 1]); (1, [Elem 0; Elem 0])];
        [epsilon]
      |];
      names = [|"BinNode"; "Leaf"|]
    } in
  Alcotest.check grammar "binary" expected (parse "binary")

let nary () =
  let expected = Grammar.{
      rules = [|
        [(1, [Elem 1])];
        [(0, [Elem 2]); (0, [Elem 0; Elem 1])];
        [epsilon]
      |];
      names = [|"NTree"; "Seq"; "Leaf"|]
    } in
  Alcotest.check grammar "nary" expected (parse "nary")

let seq () =
  let expected = Grammar.{
      rules = [|[(1, [Seq 0])]|];
      names = [|"Node"|]
    } in
  Alcotest.check grammar "seq" expected (parse "seq")

let seq2 () =
  let expected = Grammar.{
      rules = [|
        [(1, [Elem 1])];
        [epsilon; (0, [Elem 0; Elem 1])];
      |];
      names = [|"Node"; "Seq"|]
    } in
  Alcotest.check grammar "seq2" expected (parse "seq2")

let shuffle_plus () =
  let expected = Grammar.{
      rules = [|
        [(0, [Elem 1]); (0, [Elem 2])];
        [(1, [Seq 0])];
        [(0, [Elem 1; Elem 1; Seq 1])];
      |];
      names = [|"A"; "Ashuffle"; "Aplus"|]
    } in
  Alcotest.check grammar "shuffle_plus" expected (parse "shuffle_plus")

let sp () =
  let expected = Grammar.{
      rules = [|[(1, []); (1, [Elem 0]); (1, [Elem 0; Elem 0; Elem 0])]|];
      names = [|"T"|]
    } in
  Alcotest.check grammar "sp" expected (parse "sp")

let unarybinary () =
  let expected = Grammar.{
      rules = [|[(1, []); (1, [Elem 0]); (1, [Elem 0; Elem 0])]|];
      names = [|"UBTree"|]
    } in
  Alcotest.check grammar "unarybinary" expected (parse "unarybinary")

let unarybinary2 () =
  let expected = Grammar.{
      rules = [|
        [(0, [Elem 3]); (0, [Elem 1]); (0, [Elem 2])];
        [(1, [Elem 0])];
        [(1, [Elem 0; Elem 0])];
        [(1, [])]
      |];
      names = [|"UBTree"; "Unary"; "Binary"; "UBLeaf"|]
    } in
  Alcotest.check grammar "unarybinary2" expected (parse "unarybinary2")


let () =
  Alcotest.run "parsing" ["parsing", [
      "Parse binary.spec", `Quick, binary;
      "Parse nary.spec", `Quick, nary;
      "Parse seq.spec", `Quick, seq;
      "Parse seq2.spec", `Quick, seq2;
      "Parse shuffle_plus.spec", `Quick, shuffle_plus;
      "Parse sp.spec", `Quick, sp;
      "Parse unarybinary.spec", `Quick, unarybinary;
      "Parse unarybinary2.spec", `Quick, unarybinary2;
    ]]

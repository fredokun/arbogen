open Arbolib


let grammar =
  let pp = Grammar.pp in
  let equal g1 g2 = g1 = g2 in (* XXX *)
  Alcotest.testable pp equal

let parse test_name =
  let _, grammar = ParseUtil.parse_from_file (test_name ^ ".spec") in
  Grammar.completion grammar


let binary () =
  let expected = Grammar.[
      "BinNode", [
        (1, [Elem "Leaf"]);
        (1, [Elem "BinNode"; Elem "BinNode"])
      ];
      "Leaf", [epsilon]
    ] in
  Alcotest.check grammar "binary" expected (parse "binary")

let nary () =
  let expected = Grammar.[
      "NTree", [(1, [Elem "Seq"])];
      "Seq", [
        (0, [Elem "Leaf"]);
        (0, [Elem "NTree"; Elem "Seq"])
      ];
      "Leaf", [epsilon]
    ] in
  Alcotest.check grammar "nary" expected (parse "nary")

let seq () =
  let expected = Grammar.[
      "Node", [(1, [Seq "Node"])]
    ] in
  Alcotest.check grammar "seq" expected (parse "seq")

let seq2 () =
  let expected = Grammar.[
      "Node", [(1, [Elem "Seq"])];
      "Seq", [
        (0, []);
        (0, [Elem "Node"; Elem "Seq"])
      ];
    ] in
  Alcotest.check grammar "seq2" expected (parse "seq2")

let shuffle_plus () =
  let expected = Grammar.[
      "A", [
        (0, [Elem "Ashuffle"]);
        (0, [Elem "Aplus"])
      ];
      "Ashuffle", [(1, [Seq "A"])];
      "Aplus", [(0, [Elem "Ashuffle"; Elem "Ashuffle"; Seq "Ashuffle"])];
    ] in
  Alcotest.check grammar "shuffle_plus" expected (parse "shuffle_plus")

let sp () =
  let expected = Grammar.[
      "T", [
        (1, []);
        (1, [Elem "T"]);
        (1, [Elem "T"; Elem "T"; Elem "T"])]
    ] in
  Alcotest.check grammar "sp" expected (parse "sp")

let unarybinary () =
  let expected = Grammar.[
      "UBTree", [
        (1, []);
        (1, [Elem "UBTree"]);
        (1, [Elem "UBTree"; Elem "UBTree"])]
    ] in
  Alcotest.check grammar "unarybinary" expected (parse "unarybinary")

let unarybinary2 () =
  let expected = Grammar.[
      "UBTree", [
        (0, [Elem "UBLeaf"]);
        (0, [Elem "Unary"]);
        (0, [Elem "Binary"])
      ];
      "Unary", [(1, [Elem "UBTree"])];
      "Binary", [(1, [Elem "UBTree"; Elem "UBTree"])];
      "UBLeaf", [(1, [])]
    ] in
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

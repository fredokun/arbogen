open Arbolib


let grammar =
  let pp = Grammar.pp in
  let equal g1 g2 = g1 = g2 in (* XXX *)
  Alcotest.testable pp equal

let parse test_name =
  let _, ast = ParseUtil.parse_from_file (test_name ^ ".spec") in
  Ast.grammar_of_ast_grammar ast


let binary () =
  let expected = Grammar.[
      "BinNode", [
        Cons (1, [Elem "Leaf"]);
        Cons (1, [Elem "BinNode"; Elem "BinNode"])
      ];
      "Leaf", [epsilon]
    ] in
  Alcotest.check grammar "binary" expected (parse "binary")

let nary () =
  let expected = Grammar.[
      "NTree", [Cons (1, [Elem "Seq"])];
      "Seq", [
        Call "Leaf";
        Cons (0, [Elem "NTree"; Elem "Seq"])
      ];
      "Leaf", [epsilon]
    ] in
  Alcotest.check grammar "nary" expected (parse "nary")

let seq () =
  let expected = Grammar.[
      "Node", [Cons (1, [Seq "Node"])]
    ] in
  Alcotest.check grammar "seq" expected (parse "seq")

let seq2 () =
  let expected = Grammar.[
      "Node", [Cons (1, [Elem "Seq"])];
      "Seq", [
        Cons (0, []);
        Cons (0, [Elem "Node"; Elem "Seq"])
      ];
    ] in
  Alcotest.check grammar "seq2" expected (parse "seq2")

let shuffle_plus () =
  let expected = Grammar.[
      "A", [Call "Ashuffle"; Call "Aplus"];
      "Ashuffle", [Cons (1, [Seq "A"])];
      "Aplus", [Cons (0, [Elem "Ashuffle"; Elem "Ashuffle"; Seq "Ashuffle"])];
    ] in
  Alcotest.check grammar "shuffle_plus" expected (parse "shuffle_plus")

let sp () =
  let expected = Grammar.[
      "T", [
        Cons (1, []);
        Cons (1, [Elem "T"]);
        Cons (1, [Elem "T"; Elem "T"; Elem "T"])]
    ] in
  Alcotest.check grammar "sp" expected (parse "sp")

let unarybinary () =
  let expected = Grammar.[
      "UBTree", [
        Cons (1, []);
        Cons (1, [Elem "UBTree"]);
        Cons (1, [Elem "UBTree"; Elem "UBTree"])]
    ] in
  Alcotest.check grammar "unarybinary" expected (parse "unarybinary")

let unarybinary2 () =
  let expected = Grammar.[
      "UBTree", [Call "UBLeaf"; Call "Unary"; Call "Binary"];
      "Unary", [Cons (1, [Elem "UBTree"])];
      "Binary", [Cons (1, [Elem "UBTree"; Elem "UBTree"])];
      "UBLeaf", [Cons (1, [])]
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

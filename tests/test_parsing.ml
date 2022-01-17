let grammar =
  let pp = Grammar.pp in
  let equal g1 g2 = g1 = g2 in (* XXX *)
  Alcotest.testable pp equal

let parse test_name =
  let _, grammar = Frontend.parse_from_file (test_name ^ ".spec") in
  grammar


let binary () =
  let expected = Grammar.{
    names = [|"BinNode"; "Leaf"|];
    rules = [|
      Union (
        Product (Ref 1, Z 1),
        Product (Product (Ref 0, Ref 0), Z 1));
      Z 0;
    |];
  } in
  Alcotest.check grammar "binary" expected (parse "binary")

let nary () =
  let expected = Grammar.{
    names = [|"NTree"; "Seq"; "Leaf"|];
    rules = [|
      Product (Z 1, Ref 1);
      Union (Ref 2, Product (Ref 0, Ref 1));
      Z 0;
    |];
  } in
  Alcotest.check grammar "nary" expected (parse "nary")

let seq () =
  let expected = Grammar.{
    names = [|"Node"|];
    rules = [|Product (Seq (Ref 0), Z 1)|];
  } in
  Alcotest.check grammar "seq" expected (parse "seq")

let seq2 () =
  let expected = Grammar.{
    names = [|"Node"; "Seq"|];
    rules = [|
      Product (Ref 1, Z 1);
      Union (Z 0, Product (Ref 0, Ref 1));
    |];
  } in
  Alcotest.check grammar "seq2" expected (parse "seq2")

let shuffle_plus () =
  let expected = Grammar.{
    names = [|"A"; "Ashuffle"; "Aplus"|];
    rules = [|
      Union (Ref 1, Ref 2);
      Product (Seq (Ref 0), Z 1);
      Product (Product (Ref 1, Ref 1), Seq (Ref 1));
    |];
  } in
  Alcotest.check grammar "shuffle_plus" expected (parse "shuffle_plus")

let sp () =
  let expected = Grammar.{
    names = [|"T"|];
    rules = [|
      Union (
        Union (
          Z 1,
          Product (Z 1, Ref 0)
        ),
        Product (Product (Product (Z 1, Ref 0), Ref 0), Ref 0)
      )
    |];
  } in
  Alcotest.check grammar "sp" expected (parse "sp")

let unarybinary () =
  let expected = Grammar.{
    names = [|"UBTree"|];
    rules = [|
      Union (
        Union (
          Z 1,
          Product (Ref 0, Z 1)
        ),
        Product (Product (Ref 0, Ref 0), Z 1)
      )
    |];
  } in
  Alcotest.check grammar "unarybinary" expected (parse "unarybinary")

let unarybinary2 () =
  let expected = Grammar.{
    names = [|"UBTree"; "Unary"; "Binary"; "UBLeaf"|];
    rules = [|
      Union (Union (Ref 3, Ref 1), Ref 2);
      Product (Ref 0, Z 1);
      Product (Product (Ref 0, Ref 0), Z 1);
      Z 1;
    |];
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

// Grammar file for binary trees (counting internal nodes)

Bintree ::=  <1> + Bintree * Bintree * <z>

/* Variant : binary trees (counting leaves)

Bintree ::= <z> + Bintree * Bintree

*/

/* Variant: binary trees (counting leaves once and internal nodes twice)

Bintree ::= Leaf * <z> + Bintree * Bintree * <z> * <z>

*/

/* Tree generation :

arb. compact format

Bintree[Bintree[],Bintree[Bintree[],Bintree[Bintree[],Bintree[Bintree[],Bintree[]]]]]

xml. format

<?xml version="1.0"?>
<tree>
  <node type="Bintree">
    <node type="Bintree"></node>
    <node type="Bintree">
      <node type="Bintree"></node>
      <node type="Bintree">
        <node type="Bintree"></node>
        <node type="Bintree">
          <node type="Bintree"></node>
          <node type="Bintree"></node>
        </node>
      </node>
    </node>
  </node>
</tree>

dot file  (with or without labels) :

digraph {
  8 [label="Bintree"];
  0 [label="Bintree"];
  7 [label="Bintree"];
  1 [label="Bintree"];
  6 [label="Bintree"];
  2 [label="Bintree"];
  5 [label="Bintree"];
  3 [label="Bintree"];
  4 [label="Bintree"];
  8 -> 0;
  8 -> 7;
  7 -> 1;
  7 -> 6;
  6 -> 2;
  6 -> 5;
  5 -> 3;
  5 -> 4;
}

*/

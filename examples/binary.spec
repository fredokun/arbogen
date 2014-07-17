
// grammar file for binary trees (counting leaves)

BinNode ::=  Leaf * <z> + BinNode * BinNode * <z>

/* variant : binary trees (counting internal nodes)

BinNode ::= Leaf +  BinNode * BinNode * <z>

*/

/* variant : binary trees (counting nodes)

BinNode ::= Leaf * <z> + BinNode * BinNode * <z>

*/

/* variant : binary trees (counting differently leaves and nodes)

BinNode ::= Leaf * <z> + BinNode * BinNode * <z> * <z>

*/

/* Tree generation :

arb. compact format

BinNode[Leaf,BinNode[Leaf,Leaf]]

xml. format

<xml ...>
<tree>
<node name="BinNode">
  <leaf name="Leaf"/>
  <node name="BinNode">
    <leaf name="Leaf"/>
    <leaf name="Leaf"/>
  </node>
</node>  
</tree>

dot file  (with or without labels) :

digraph {
  // nodes
  node N1 [label="BinNode"]
  node N2 [label="Leaf"]
  node N3 [label="BinNode"]
  node N4 [label="Leaf"]
  node N5 [label="Leaf"]
  // edges
  N1 -> N2
  N1 -> N3
  N3 -> N4
  N3 -> N5
}

*/

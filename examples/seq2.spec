
// grammar file for binary trees (counting leaves and internal nodes)
// with parameters to obtain trees of size about 100000

set min 20
set max 2000
set try 100
set zstart 0.5

Node ::= Seq * <z>
Seq ::= <1> + Node * Seq

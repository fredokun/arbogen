
// grammar file for binary trees (counting leaves and internal nodes)
// with parameters to obtain trees of size up to 1 billion nodes
// (for now, trees of about 10.000.000 nodes can be generated in a few seconds
// using this grammar).

set min 10000000
set max 1000000000
set try 10000

BinNode ::=  Leaf * <z> + BinNode * BinNode * <z>

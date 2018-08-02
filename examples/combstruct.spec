
// grammar file for binary trees (counting leaves and internal nodes)
// with parameters to obtain trees of size about 100000

set min 100000
set max 120000
set try 5000

BinNode =  Union(Leaf, Prod(BinNode, BinNode, Z))


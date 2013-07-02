
// grammar file for binary trees (counting leaves and internal nodes)
// with parameters to obtain trees of size about 100000

set min 100;
set max 200;
set try 500;

Node ::=  Leaf + Node * Node *  SEQ(Node) * <z>;

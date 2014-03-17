
// grammar file for binary trees (counting leaves and internal nodes)
// with parameters to obtain trees of size about 100000

set zstart 0.01;
set min 10;
set max 200;
set try 50000;

Tree ::=   Serie + Parallel ;
Serie ::=  Leaf * <z> + P * P * SEQ(P);
P ::= Parallel + Leaf * <z>;
Parallel ::= Leaf * <z> +  S * S * SEQ(S);
S ::= Serie + Leaf * <z>;


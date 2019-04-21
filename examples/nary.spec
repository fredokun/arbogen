
// grammar file for n-ary trees

set zstart 0.01
set min 10
set max 200
set try 50000

NTree ::= <z> * Seq
Seq ::= Leaf + NTree * Seq

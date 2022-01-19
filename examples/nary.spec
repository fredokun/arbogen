// grammar file for n-ary trees

NTree ::= <z> * Seq
Seq ::= Leaf + NTree * Seq

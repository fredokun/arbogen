
// grammar file for n-ary trees

NTree ::= Leaf * <z> + Seq;
Seq ::= Ntree * Seq * <z>;


// grammar file for plane trees

Node ::= Seq * <z>
Seq ::= <1> + Node * Seq

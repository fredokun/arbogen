
// grammar file for unary/binary trees

set min 1000 ;
set max 10000 ;
set try 1000 ;

UBTree ::= Leaf + Unary + Binary;
Unary ::= UBTree * <z>;
Binary ::= UBTree * UBTree * <z>;
Leaf ::= 1 * <z>;

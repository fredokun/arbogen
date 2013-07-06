
// grammar file for unary/binary trees

set min 1000 ;
set max 10000 ;
set try 40 ;

UBTree ::= Leaf * <z> + UBTree * <z> + UBTree * UBTree * <z>;

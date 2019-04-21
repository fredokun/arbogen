// grammar file for unary/binary trees

UBTree ::= UBLeaf + Unary + Binary
Unary ::= UBTree * <z>
Binary ::= UBTree * UBTree * <z>
UBLeaf ::= <z>

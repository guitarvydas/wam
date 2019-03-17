WAM is essentially a byte-code compiler and interpreter.

As compilation proceeds, byte-codes are shoved (linearly, as a stream) into the "HEAP".

L0 compilation of queries uses 3 bytecodes:

(NB, the text refers to the HEAP as a stack, but it is not used as a push/pop stack, it has only one operation "push")

1. put_structure 
   - places a <STR,addr> bytecode into the heap
   - if the structure has not yet been seen, then it is compiled into the heap immediately after the <STR,addr> bytecode
      -- a <name-pointer,arity> bytecode
      -- the rest of the terms belonging to the structure (constants, Variables, nested structs) compiled to bytecodes
   - if the structure has been seen, then there are no succeeding bytecodes and the addr part of the <STR,addr> bytecode points backwards in the heap to the address of the already-seen structure

2. set_variable
   - create a (logic) variable <REF,addr> in the heap
   - copy it into the given register <REF,addr> (I've broken this into 2 bytecodes, one for Xi and another for Yi)

3. set_value
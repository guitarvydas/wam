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


PUT and GET bytecodes

In general, "Put" bytecodes move items into the argument (Ai) registers.

In general, "Get" bytecodes move items from the argument (Ai) registers.


EXAMPLE:

LTEST0:

prolog:
  father(son-of-paul, paul).
  ?- fathter(X,paul).

intermediate sexprs:
  (wam:defrel father
          ((father son-of-paul paul)))
  (wam:?- (father ?X paul))

emitted WAM code:
$FATHER/2
  WAM::GET-CONSTANT "SON-OF-PAUL" 1 
  WAM::GET-CONSTANT "PAUL" 2 
  WAM::PROCEED 
$QUERY/0
  WAM::ALLOCATE 
  WAM::PUT-Y-VARIABLE 1 1 
  WAM::PUT-CONSTANT "PAUL" 2 
  WAM::CALL ?FATHER/2 1 
  WAM::DONE 
  WAM::DEALLOCATE 

explanation of emitted code:
"$" is a label
"?" is a reference to a label

1. The $FATHER code says to get the constant "SON-OF-PAUL from A1.  If A1 contains a prolog variable (<REF,addr>), then unify the variable with the constant.

2. Then,  get the constant "PAUL" from A2.

3. PROCEED: End the current execution leg.  In this case, there is only one leg. 

$query/0

4. Allocate a new frame.  bug? Allocate should be told to allocate 1 local

5. Create a Variable <REF,addr> unbound in the 1st local and 

6. Call ?FATHER/2 and leave 1 local on the stack afterwards.

7. DONE is not defined by WAM.  It is a utility byte-code in this implementation.

8. DEALLOCATE - pops the environment (for $query/0) off of the stack, removing local variables in the process.


LTEST1:

prolog:

father(paul,father_of_paul).
father(son_of_paul,paul).
father(daughter_of_paul,paul).
?- father(X,Y).

sexprs:
  (wam:defrel father
          ((father paul father-of-paul))
          ((father son-of-paul paul))
          ((father daughter-of-paul paul)))
  (wam:?- (father ?X ?Y)))

WAM:
$FATHER/2
  WAM::TRY-ME-ELSE ?L1 
  WAM::GET-CONSTANT "PAUL" 1 
  WAM::GET-CONSTANT "FATHER-OF-PAUL" 2 
  WAM::PROCEED 
$L1
  WAM::RETRY-ME-ELSE ?L2 
  WAM::GET-CONSTANT "SON-OF-PAUL" 1 
  WAM::GET-CONSTANT "PAUL" 2 
  WAM::PROCEED 
$L2
  WAM::TRUST-ME 
  WAM::GET-CONSTANT "DAUGHTER-OF-PAUL" 1 
  WAM::GET-CONSTANT "PAUL" 2 
  WAM::PROCEED 
$QUERY/0
  WAM::ALLOCATE 
  WAM::PUT-Y-VARIABLE 1 1 
  WAM::PUT-Y-VARIABLE 2 2 
  WAM::CALL ?FATHER/2 2 
  WAM::DONE 
  WAM::DEALLOCATE 

almost the same as above, except with backtracking "legs" TRY-ME-ELSE, RETRY-ME-ELSE, TRUST-ME.


LTEST7:

prolog:
s(a,b,c).
?- s(X).

sexprs:
  (wam:defrel s ((s #(a b c))))
  (wam:?- (s ?X))))

WAM:

$S/1
  QUOTE 
  WAM::GET-STRUCTURE "A/2" 1 
  WAM::UNIFY-CONSTANT "B" 
  WAM::UNIFY-CONSTANT "C" 
  WAM::PROCEED 
$QUERY/0
  WAM::ALLOCATE 
  WAM::PUT-Y-VARIABLE 1 1 
  WAM::CALL ?S/1 1 
  WAM::DONE 
  WAM::DEALLOCATE 

# Code generation and specific optimisations for the PilGRIM


### Generating instructions from the assembly language
The process of generating actual instructions from the assembly language is straightforward.
First, every constructor is assigned a unique number, where the numbers are grouped in such a way, that the lowest bits distinguish between the alternatives within a data type.
Values on the node stack are indexed from the top by a number starting with zero, and values in the queues are indexed from the most recently produced entry in the queue.
Every instruction that produces a result pushes its result on either the node stack or one of the queues.
Thus the sequence of executed instructions within a supercombinator determines the translation of variables in the assembly to stack or queue indices (without any register allocation).

For accurate garbage collection, the assembler must set the bits to pop nodes from the stack or to clear the reference queue.
To minimize the risk of space leaks, the assembler can set these bits in every instruction.

Another task of the assembler is to keep all required values on accessible positions.
The stack (locally to a supercombinator) can be at most four entries deep.
Therefore, before every function call or evaluation, values that are used later are pushed from the queues onto the node stack.
This is performed by pushing a node combining values from several other nodes on the stack or by spilling values not used directly to the heap. 

After generating all instructions in a supercombinator, the if and case instructions in it are given jump offsets for the else branch and case alternatives, respectively.
The last step is linking together all supercombinators, which assigns an instruction address to every use of a function name.

### Dealing with architectural limitations
maximum node size/number of function arguments: work around that in code generator see bwm and Reduceron papers   
maximum number of application arguments pushed in parallel: can push multiple sets of arguments to the continuation stack   
number of values read in a single instructions: (limited by node size or ??)   
depth of the node stack that is directly accessible, compacting the stack by pushing and popping or explicit spilling to heap   
depth of reference queue and prim queue: pushing values onto the node stack before falling out the queues   
dealing with case statements with a very large number of alternatives: adding big case instruction that does a relative jump with index of the constructor alternative times a constant   
updating a small F-node with a big result node: using indirection nodes


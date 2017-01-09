## Instruction set encoding

All instructions can be encoded in 128 bits though the majority of instructions fit within 64 bits.
The mix of two instruction lengths is a trade off between fast (fixed length) instruction decoding and small code size (where variable length instructions are better).
All instructions have 16 header bits in a common structure, where the first bit differentiates between small and big instructions.
The following 10 bits contain the opcode of the instruction.
The last five bits in the header are for cleaning up the stack; Consisting of a 4-bit bitmask to optionally pop any of the top four stack nodes, and one bit to clear the reference queue.

Big instructions have the same structure: 16-bit header, 16 bits of instruction specific information, a 32-bit constant value (function address, heap constant address or a constructor index) and the remaining 64 bits are 8 operands.
Depending on the instruction, the operands have different purposes: function or constructor arguments, a value to be evaluated, extra application arguments or, for case instructions, jump offsets for each case alternative.
When the number of normal arguments plus the number of alternatives in a case instruction exceed the 8 available operands, an alternative encoding is used where the jump offsets for alternatives are fixed constants.
The disadvantage of using fixed jump offsets is less compact binary code.

Many instructions that do not need many operands can be encoded in 64 bits.
The first category of small instructions are the ones with no operands at all, such as pushing constants or CAF references, and stores and returns of nullary nodes.
Secondly, instructions with 5 operands or less, that require little other metadata, fit in 64 bits.
Examples are Eval combined with a Call or Jump and Case of Eval with four or less alternatives.
The third category of small instructions are all primitive operations, the if-with-compare and arithmetic operations.
If the cost in speed and complexity of the instruction decoder does not become too high other instruction variants could be considered for the compact encoding.

Operands are encoded in 8 bits.
The first bit distinguishes between primitive and reference values, the second bit whether it is read from the node stack or not.
For values read from the stack, two bits select one of the top four nodes and the next three select which element in a node is read.
For values not read from the stack, four bits are used to index the reference or primitive queue.
Instead of reading a value, operands can contain a four bit constant value.
For reference values, copying/destructive reads are explicitly encoded, to have accurate live reference information for garbage collection.


argument byte:
```
7  6  5  4  3  2  1  0
ref  T/C
0  0  0  -si-  -nelem-
0  0  1  -si-  -nelem-
0  1  0  0  -queue_ix-
0  1  1  0  -queue_ix-
0  1  0  1  C_constVal
0  1  1  1              % for future use
```

primitive argument:
```
1  0  0  -si-  -nelem-
1  0  1  -valsi-  -ix-  % if using value stack
1  1  0  0  -queue_ix-
1  1  0  1  int_const
1  1  1                 % maybe like the previous two but then for floats
```

local primimitive (6 bits):
```
1  -valsi-  -ix-
0  0  -queue_ix-
0  1  int_const
```

```
127 126 125 124..117  116..112  111..109  108..96  95..64    63..56 55..48 47..40 39..32 31..24 23..16 15..8  7..0
 1   0   0  store_C   pop_mask   n_args      _     C_index    argA   argB    ..     ..     ..     ..    argG    _
 1   0   0  store_F      ..        ..     tagMeta  F_addr      ..                   ..                   ..
 1   0   0  store_P      ..        ..     tagMeta  F_addr      ..                   ..                   ..
 1   0   0  push         ..        ..        _        _
 1   0   1  return_C     ..        ..        _     C_index     ..                   ..                   ..
 1   0   1  return_P     ..        ..     tagMeta  F_addr      ..                   ..                   ..
```

```
127 126 125 124..121 120..117  116..112  111..109 108..100   99..96  95..64    63..56 55..48 47..40 39..32 31..24 23..16 15..8  7..0
 1   1   0   jump      eval    pop_mask     _        _     contmeta    _        argA   argB    ..     ..     ..     ..     ..   argH
 1   1   0   jump      fetch      ..        _        _     contmeta    _         ..                   ..                         ..
 1   1   0   jump     evalcaf     ..      --pointer_tag--  contmeta  heap_addr   ..                   ..                         .. 
 1   1   0   jump      tlf        ..      n_args     _     contmeta  F_addr      ..                   ..                         ..
 1   1   0   jump      fix        ..      n_args     _     contmeta  F_addr      ..                   ..                         ..
 1   1   1   call      eval       ..        _        _        ..       _         ..                   ..                         .. 
                                                                     
 1   1   1   force     eval       ..        _        _        ..       _         ..                   ..                         .. 
                                                                     
 1   1   1   case2     eval       ..        _        _        ..       _         ..                   ..                         .. 
                                                                     
 1   1   1   case4     eval       ..        _        _        ..       _         ..                   ..                         .. 
                                                                     
 1   1   1   case8     eval       ..        _        _        ..       _         ..                   ..                         .. 
                                                                     
 1   1   1   casebig   eval       ..        _        _        ..       _         ..                   ..                         .. 
```

args as case alt:
```
  7..6      5..0     
elem to     jump  (elem 0 is no eval next)
eval next  offset (offset 0 is return top) 
```

```
63 62 61  60..53      52..48   47..32   31..0
0  0  0  push_caf    pop_mask  ptrtag   heap_addr
0  0  0  bigconst       ..     -integer_value-
0  0  0  store_P0       ..     tagMeta  F_addr
0  0  0  store_C0       ..        _     C_index
0  0  0  returnP0       ..     tagMeta  F_addr
0  0  0  returnC0       ..        _     C_index
0  0  0  pushboxconst   ..     boxTag   int_value
0  0  0  retboxconst    ..     boxTag   int_value

0  0  0  jump_caf  pop_mask  ptrtag   heap_addr
0  0  0  call_caf     ..     ptrtag   heap_addr

0  0  0  directjmp    ..        _     code_addr
```

```
63 62 61  60..53     52..48   47..24   23..16 15..8  7..0
0  0  1   storeC13  pop_mask  C_index   argA   argB  argC
0  0  1   storeFSB     ..     BF_meta    ..     ..    ..
0  0  1   storePSB     ..     BP_meta    ..           ..
0  0  1   returnCn     ..     C_index    ..           ..
0  0  1   returnPS     ..     BP_meta    ..           ..
0  0  1   smallpush    ..        _       ..           ..

0  0  1   throw        ..        _      argA    _      _

0  0  1   jumptlf      ..     reloffset  ..     ..    ..
0  0  1   calltlf      ..     reloffset  ..     ..    ..
```

```
63 62 61  60..53      52..48   47..40 39..32 31..24 23..16 15..8  7..0

0  1  0   pushcont   pop_mask contmeta  argA  argB   argC   argD  argE
0  1  0   smallpush    ..      n_args    ..    ..     ..    ..     ..

0  1  0   jumpeval     ..     contmeta   ..    ..     ..    ..     ..
0  1  0   calleval     ..     contmeta   ..           ..           ..
0  1  0   forceval     ..     contmeta   ..           ..           ..

0  1  0   jumptlf      ..     reloffset  ..           ..           ..
0  1  0   calltlf      ..     reloffset  ..           ..           ..

0  1  0   caseeval2    ..     contmeta  argA  argB   argC   altA  altB
0  1  0   caseeval4    ..        _      argA  altA   altB   altC  altD
```

```
63 62 61   60..53     52..48   47..38 37..31 30..24 23..14 13..7 6..0
0  1  1   if_w_cmp   pop_mask  cmp_op  argA   argB   -branch_offset-
0  1  1   primprim      ..     bin_op  argA   argB  bin_op  argC argD
0  1  1   sconprim      ..       -integer_value-    bin_op  argC argD
0  1  1   primscon      ..     bin_op  argA   argB   -integer_value-
```

# ALFIN: Another Lazy Functional Intermediate Notation

##### Choosing for a new intermediate language

pick best aspects from STG and GRIN
independent of target platform

### Design an intermediate notation
only the control expression and the top of stack determine which abstract machine rule applies

##### Heap representation
uniform header plus list of arguments
 * F-nodes
 * P-nodes
 * C-nodes
each argument explicitly either reference or prim value


##### Simple example program
```
main = sum (map double [0..10])
       where double x = x*2

main =
   e <- Store (Fn_enumFromTo' 0# 10#);
   d <- Store (P1_double);
   m <- Store (Fn_map d e);
   (C_Int s#) <= Call (sum 0# m);
   (C_Unit) <= IO (putInt s#);
   Jump IO (endProgram)

enumFromTo' n# m# =
   g? <- Prim (intGT n# m#);
   if g? then
      Return (C_Nil)
   else
      n <- Store (C_Int n#);
      k# <- Prim (intAdd n# 1#);
      e <- Store (Fu_enumFromTo' k# m#);
      Return (C_Cons n e)

map f xs =
   Case (Eval xs) of
      (C_Nil) -> Return (C_Nil)
      (C_Cons y ys) ->
         z <- Store (Fu_Ap1 f y);
         zs <- Store (Fu_map f ys);
         Return (C_Cons z zs)

sum' t# xs =
   Case (Eval xs) of
      (C_Nil) -> Return (C_Int t#)
      (C_Cons y ys) ->
         C_Int n# <= Call (Eval y);
         s# <- Prim (intAdd t# n#);
         Jump Call (sum' s# ys)

double x =
   (C_Int n#) <= Call (Eval x);
   d# <- Prim (intMul n# 2#);
   Return (C_Int d#)
```
##### Data representation on heap and stack
only nodes on heap
nodes with attached reference on stack

##### Basic functional constructs
every function returns a node in WHNF

return
store
eval
update
case
call
partial apply

##### Non functional constructs
if as specialisation of case / explicit booleans
switch on numbers??
primops
if / switch
io prims
throw / catch

##### Additions for a practical functional language
annotations
selectors
cafs
recursive lets

##### Compiler annotation for efficiency
distinction between eval and fetch
exact applications

##### ALFIN syntax
```
node ::= header arg*

header ::= con | fun | pap | Ind | Blackhole
con ::= C_cname
fun ::= F(u|n)_fname
pap ::= Pm_fname

prog ::= def+

def ::= fname var* (Fix rvar)? = block

arg ::= ref | prim

prim ::= pvar | num

ref ::= rvar | CAF fname

var ::= rvar | pvar | _

block ::= (stat;)* term

stat ::= rvar <- Store node
       | pvar <- Prim (pname arg*)
       | match <= call cont*  -- TODO make this syntactic sugar of single alt case?

call  ::= Eval ref
        | Fetch ref
        | Call (fname arg*)
        | Fix (fname arg*)
        | IO (pname arg*)

cont ::= Apply ref+
       | Select i
       | Catch ref

term ::= Return node
       | Jump call cont*
       | Case call cont* Of calt+
       | If pvar then block else block
       | Switch pvar Of salt+
       | Throw ref

calt ::= match -> block

match ::= rvar
        | con var*
        | rvar @ con rvar*

salt ::= (num | Default) -> block
```

##### Potential for extensions
partially applied constructors
annotate exact applications
more builtin functions
other kinds of continuations
floating point / vector primitives
special calling conventions


### From Core to ALFIN and its semantics

##### ALFIN semantics
```
<H|L|R> <heap+external|local environment|return/continuation stack>
L = (N/r/p)*
R = (C | Update r | RCase L as) , R

I is  instruction
C c   call
E     eval
R     return/continue
M     match
U e   unwind

_x value of x
&x name/location of x

_x,H' = allocate (H, _N)
_N,H' = retrieve (H, _x)

instruction semantics
I &x <- Store N; is   <H|L|R> ~~> I is <H'|x,L|R>       where (_x,H') = allocate (H,_N)
I &y <- Prim p xs; is <H|L|R> ~~> I is <H |y,L|R>       where Res _y = runPrim p _xs
                              ~~> U _e <H |e  |R>       where Exc _e = runPrim p _xs
I m <= Call c,ec; is  <H|L|R> ~~> C _c <H |e  |_ec;Rcase L {m->is},R>
I Return N            <H|L|R> ~~> R _N <H |e  |R>
I Jump c,ec           <H|L|R> ~~> C _c <H |e  |_ec;R>
I Case c,ec as        <H|L|R> ~~> C _c <H |e  |_ec;Rcase L as,R>
I If p xs ys          <H|L|R> ~~> I is <H |L  |R>       where is = if _p then xs else ys
I Throw e             <H|L|R> ~~> U _e <H |e  |R>

call semantics
C (Eval _x)    <H|e|R>  ~~> E _N <H'|e       |R>  where _N,H' = retrieve (H, _x)
C (Fetch _x)   <H|e|R>  ~~> R _N <H'|e       |R>  where _N,H' = retrieve (H, _x)
C (Call f _xs) <H|e|R>  ~~> I is <H |Fnf xs  |R>  where (&xs, is) = lookup (f);
C (Fix f _xs)  <H|e|R>  ~~> I is <H'|r@Fnf xs|Update _r,R>  where (&r:&xs, is) = lookup (f);            (_r,H') = allocate (H, Blackhole)
C (IO p _xs)   <H|e|R>  ~~> E _N <H'|e       |R>  where _N,H' = runIO p _xs H

eval semantics
E _@(Fn_f _xs)  <H|e|R> ~~> I is <H|Fnf xs|R>           where &xs,is = lookup(f);
E _r@(Fu_f _xs) <H|e|R> ~~> I is <H|Fnf xs|Update _r,R> where &xs,is = lookup(f);           -- optionally   H' = replace (H, _r, Blackhole)
E _@(Ind _x)    <H|e|R> ~~> E _N <H'|e|R>               where _N,H' = retrieve (H, _x)
E _@(Blackhole) <H|e|R> ~~> U _x <H|e|R>                where _x = Exception "<loop>"
E  _N           <H|e|R> ~~> R _N <H|e|R>

return/continuation semantics
R _@(C_c _xs)  <H|e|Select i,  R> ~~> E _N         <H'|e|R> where _N,H' = retrieve(H, _xs#i)
R _@(Pm_f _xs) <H|e|Apply _as, R> ~~> I is     <H|Fnf ys|R> where (&ys, is) = lookup (f);      _ys = _xs++_as;  if m == |as|
                                  ~~> R (Pn_f _ys) <H |e|R> where _ys = _xs++_as;              n = m - |as|  ;  otherwise
R _r@_N        <H|e|Update _u, R> ~~> R _r@_N      <H'|e|R> where _v,H' = replace(H, _u, _N)
R _@_N         <H|e|Update _u, R> ~~> R _v@_N      <H'|e|R> where _v,H' = replace(H, _u, _N)
R _N           <H|e|Catch  _h, R> ~~> R _N         <H |e|R>
R _N           <H|e|RCase L as,R> ~~> M _N as      <H |L|R>
R _N           <H|e|e>            ~~> <<end of program>> with result _N

matching semantics
M _r@(C_c _xs)  (&r@C_c &xs-> is),as <H|L|R> ~~> I is <H |r@(C_c xs),L|R>
M (C_c _xs)     (&r@C_c &xs-> is),as <H|L|R> ~~> I is <H'|r@(C_c xs),L|R>                      where _r,H' = allocate (H, C_c _xs)
M _@(C_c _xs)   (   C_c &xs-> is),as <H|L|R> ~~> I is <H |C_c xs    ,L|R>
M _r@(C_c _xs)  (&r        -> is),as <H|L|R> ~~> I is <H |r         ,L|R>
M (C_c _xs)     (&r        -> is),as <H|L|R> ~~> I is <H'|r         ,L|R>                      where _r,H' = allocate (H, C_c _xs)
M _r@(Pm_f _xs) (&r        -> is),as <H|L|R> ~~> I is <H |r         ,L|R>
M (Pm_f _xs)    (&r        -> is),as <H|L|R> ~~> I is <H'|r         ,L|R>                      where _r,H' = allocate (H, Pm_f _xs)
M _N                            a,as <H|L|R> ~~> M _N as <H|L|R>

unwinding semantics
U  _e <H|e|Catch _h,R>  ~~> E _N <H|e|Apply _e,R>  where _N,H' = retrieve (H, _h)
U  _e <H|e|c,R>         ~~> U _e <H|e|R>
U  _e <H|e|e>           ~~> <<uncaught exception>> _e
```
##### A Lazy functional core language
in ANF form
explicit strictness
monadic expressions
```
topexp ::= let var = subexp; topexp
         | letrec {var = subexp}+ topexp
         | subexp
         | case subexp of (pat -> topexp)+
         | if var then topexp                                  -- maybe make these two
         | switch var on (num -> topexp)+ (default -> topexp)  -- a variant within case
         | throw var
         | pat <- subexp; topexp
         | return subexp

subexp ::= var
         | con arg*
         | fname arg*
         | rvar rarg*
         | sel_i rarg
         | prim_op arg*
         | io_prim arg*
         | try subexp catch rvar
```

##### Translation to ALFIN
```
toplevel expression into a statement block
T[let x = e; y]  = x <- E[e]; T[y]
T[s]             = R[s]
T[throw x]       = Throw x
T[case e of as]  = Case C[e] of A[as]
T[p <- e; y]     = p <= C[e]; T[y]
T[return s]      = R[s]

result subexpression
R[c xs] = Return C_c xs
R[f xs] = Return Pm_f xs , if arity f > |xs| where m = arity f - |xs|
R[e   ] = Jump C[e]

non strict expression evaluation
E[c xs]    = Store C_c xs
E[f xs]    = Store Fu_f xs     , if arity f == |xs|
           = Fu_Ap_n f,xs,     , if arity f == 0 where n = |xs|
           = Store Pm_f xs     where m = arity f - |xs|
E[y xs]    = Store Fu_Ap_n xs  where n = |xs|
E[sel_i x] = Store Fsel_i x
E[oper xs] = Prim oper xs

strict calling of subexpressions into a call with eval continuations
C[x]             = Eval x
C[f]             = Eval f                , if arity f == 0
C[f xs]          = Call f xs             , if arity f == |xs|
                 = Eval f, Apply xs      , if arity f == 0
                 = Call f as, Apply bs   as,bs = splitAt (arity f)
C[y xs]          = Eval y, Apply xs
C[sel_i x]       = Eval x, Select i
C[action xs]     = IO action xs
C[try e catch h] = C[e], Catch h
```
##### Example program with execution
```
fac n# =
   s? <- Prim (<=) n# 1
   If s? Then
      Return (C_Int 1)
   Else
     m# <- Prim (-) n# 1
     C_Int x# <= Call (fac m#)
     r# <- Prim (*) n# x#
     Return (C_Int r#)

C  (Call F_fac 5)                 < H | . | R >

             ~~>  &n, (s? <- Prim (<=) n# 1; is) = lookup(fac);

I  (&s <- Prim (<=) &n 1 ; is)    < H | n=5 | R >
             ~~> Res _s = runPrim (<=) 5 1

I  (If &s Then xs Else ys ; is)   < H | s=F,n=5 | R >
             ~~> is = if False then xs else ys

I  (&m <- Prim (-) &n 1 ; is)     < H | s=F,n=5 | R >
             ~~> Res _m = runPrim (-) 5 1

I  (C_Int &r <= Call (fac &m);is) < H | m=4,s=F,n=5 | R >

             ~~>  is = r# <- Prim (*) n# x# ; Return (C_Int r#)

C  (Call fac 4)                   < H | . | RCase (m=4,s=F,n=5) {C_Int &x -> is},R >
             ~~> (&n, is) = lookup(f)

I  *******                        <H | n=4 | RCase (m=4,s=F,n=5) {C_Int &x -> is},R >

-------------------------------------------------------------------------------------

I  (Return (C_Int 1))             <H | s=T,n=1 | RCase (m=1,s=F,n=2) {C_Int &x -> is},R >
             ~~>

R  (C_Int 1)                      <H | . | RCase (m=1,s=F,n=2) {C_Int &x -> is},R >
             ~~>

M  (C_Int 1) {C_Int &x -> is}     <H | m=1,s=F,n=2 | R>
             ~~>   is = r# <- Prim (*) n# x# ; Return (C_Int r#)

I  (&r <- Prim (*) &n &x ; is)   < H | x=1,m=1,s=F,n=2 | R >
             ~~> Res _r = runPrim (*) 2 1

I  (Return (C_Int &r))            < H | r=2,m=1,s=F,n=2 | R >
             ~~>

R  (C_Int 2)                      <H | . | ******* >
```

##### Embedding ALFIN in Haskell

### Conclusions

##### Lambda lifting versus free variables in closures

the choice for lambda lifting is made

analyse tradeoffs with no lambda lifting and closures with free variables

arity raising and constructor specialization introduce extra arguments


##### Reflecting on the existing intermediates.

 * ABC machine: too low level/focused on conventional processors

 * STG machine: too abstract / distinction between free variables/arguments

 * GRIN assumes full program transformations, too

 * FLite: minimal core language with limited opportunities to exploit optimizations


##### Future work
builtin arrays operations
concurrency support / asynchronous exceptions
delimited continuations
<!-- dependent type acceleration?? -->


### Large example with execution
```
zipWith f []     _      = []
zipWith f _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

tail []     = error []
tail (y:ys) = ys

indexAt n []     = error []
indexAt 0 (x:xs) = x
indexAt n (x:xs) = indexAt (n-1) xs

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main = print (fibs `indexAt` 3)

zipWith f as bs =
   Case (Eval as)
      C_Nil -> Return C_Nil;
      C_Cons x xs ->
         Case (Eval bs)
            C_Nil -> Return C_Nil;
            C_Cons y ys ->
               z <- Store (Fu_ap2 f x y)
               zs <- Store (Fu_zipWith f xs ys)
               Return (C_Cons z zs)

tail xs =
   Case (eval xs)
     C_Nil -> Throw xs
     C_Cons y ys -> Jump (Eval ys)

plus a b =
   C_Int x# <- Call (Eval a)
   C_Int y# <- Call (Eval b)
   z# <- Prim (+) x# y#
   Return (C_Int z#)

indexAt #n xs =
  Case (Eval xs)
     C_Nil -> Throw xs
     C_Cons y ys ->
        ?z <- Prim (==) n# 0
        If ?z Then
           Jump (Eval y)
        Else
           m# <- Prim (-) n# 1
           Jump (Call indexAt m# ys)

fibs res@() =
   x <- Store (C_Int 1)
   p <- Store (P2_plus)
   ts <- Store (Fu_tail res)
   zs <- Store (Fu_zipWith p res ts)
   xs <- Store (C_Cons x zs)
   Return (C_Cons x xs)

main =
   xs@_ <= Fix (fibs)
   C_Int n# <= Call (indexAt 3 xs)
   Jump (IO printInt n#)

I (&xs@_ <= Fix (fibs);js)               << H0 | . | R0 >>
             ~~>  
C (Fix (fibs))                           << H0 | . | RCase () {&xs@_ -> js},R0 >>
             ~~>  (&res, is) = lookup(fibs)  (%0, H') = allocate (H0, Hole)
I (&x <- Store (C_Int 1)                 <<%0->Hole,H0 | res=%0 | Update %0,R1 >>
             ~~>  (%1, H') = allocate (H1, C_Int 1)
I (&p <- Store (P2_plus)                 <<%1->(C_Int 1),H1 | x=%1,L1 | R2 >>
             ~~>  (%2, H') = allocate (H2, P2_plus)
I (&ts <- Store (Fu_tail &res)           <<%2->P2_plus,H2 | p=%2,L2 | R2 >>
             ~~>  (_ts, H') = allocate (H3, Fu_tail %0)
I (&zs <- Store (Fu_zipWith &p &res &ts) <<%3->Fu_tail %0,H3 | ts=%3,L3 | R2 >>
             ~~>  (_zs, H') = allocate (H4, Fu_zipWith %2 %0 %3)
I (&xs <- Store (C_Cons &x &zs)          <<%4->Fu_zipWith %2 %0 %3,H4 | zs=%4,L4 | R2 >>
             ~~>  (_xs, H') = allocate (H5, C_Cons %1 %4)
I (Return (C_Cons &x &xs))               <<%5->C_Cons %1 %4, H5 | xs=%5,L5 | R2 >>
             ~~>  
R (C_Cons %1 %5)                         <<H6 | . | Update %0,R1 >>
             ~~> (%0, H') = replace (H6, %0, C_Cons %1 %5)
R %0@(C_Cons %1 %5)                      <<%0->C_Cons %1 %5,H5 | . | RCase () {&xs@_ -> as},R0 >>
             ~~>
M %0@(C_Cons %1 %5) {&xs@_ -> as}        <<H6 | . | R0 >>
             ~~>
I (C_Int &n <= Call (indexAt 3 &xs);rs)  <<H6 | xs=%0 | R0 >>

C (Call (indexAt 3 %0))                  <<H6 | . | RCase (xs=%0) {C_Int &n -> rs},R0 >>
             ~~>   (&n &xs, is) = lookup(indexAt)
I Case (Eval xs) {as}                    <<H6 | n=3,xs%0 | R1 >>
             ~~>
C (Eval %0)                              <<H6 | . | RCase (n=3,xs%0) {as},R1 >>
             ~~> (%0@(C_Cons %1 %5), H') = retrieve (H, %0)
E+R %0@(C_Cons %1 %5)                    <<H6 | . | RCase (n=3,xs%0) {as},R1 >>
             ~~>
M %0@(C_Cons %1 %5) {C_Cons y ys -> is}  <<H6 | n=3,xs%0 | R1 >>
             ~~>
I (&z <- Prim (==) &n 0;is)              <<H6 | ys=%5,n=3,L2 | R1 >>
             ~~>   Res _z = runPrim (==) 3 0
I (If &z then as else bs)                <<H6 | z=F,ys=%5,n=3,L2 | R1 >>
             ~~>   is = if False then as else bs
I (&m <- Prim (-) &n 1);is)              <<H6 | z=F,ys=%5,n=3,L2 | R1 >>
             ~~>   Res _m = runPrim (-) 3 1
I (Jump (Call indexAt &m &ys))           <<H6 | m=2,z=F,ys=%5,L3 | R1 >>
             ~~>
C (Call (indexAt 2 %5))                  <<H6 | . | R1 >>
             ~~> (&n &xs, is) = lookup(indexAt)

***** same 8 steps as above are repeated in the execution of indexAt 2 *****

C (Call (indexAt 1 %4))                  <<H6 | . | R1 >>
             ~~>   (&n &xs, is) = lookup(indexAt)
I Case (Eval xs) {as}                    <<H6 | n=1,xs%4 | R1 >>
             ~~>
C (Eval %4)                              <<H6 | . | RCase (n=1,xs%4) {as},R1 >>
             ~~> (%4@(Fu_zipWith %2 %0 %3), H') = retrieve (H, %4)
E %4@(Fu_zipWith %2 %0 %3)               <<H6 | . | R2 >>
             ~~> (&f &as &bs, is) = lookup(zipWith)
I (Case (Eval as) {qs})                  <<H6 | f=%2,as=%0,bs=%3 | Update %4,R2 >>
             ~~>
C (Eval %0)                              <<H6 | . | RCase (f=%2,as=%0,bs=%3) {qs},R3 >>
             ~~>   (%0@(C_Cons %1 %5), H') = retrieve(H, %0)
E+R %0@(C_Cons %1 %5)                    <<H6 | . | RCase (f=%2,as=%0,bs=%3) {qs},R3 >>
             ~~>
M %0@(C_Cons %1 %5) {C_Cons &y &ys ->is} <<H6 | f=%2,as=%0,bs=%3 | R3 >>
             ~~>
I (Case (Eval &bs) {qs})                 <<H6 | x=%1,xs=%5,L3 | R3 >>
             ~~>
C (Eval %3)                              <<H6 | . | RCase (x=%1,xs=%5,L3) {qs},R3 >>
             ~~> (%3@(Fu_tail %0), H') = retrieve (H, %3)
E %3@(Fu_tail %0)                        <<H6 | . | R4 >>
             ~~> (&xs is) = lookup(tail)
I (Case (eval xs) {qs})                  <<H6 | xs=%0 | Update %3,R4 >>

C (Eval %0)                              <<H6 | . | RCase (xs%0) {qs},R5 >>
             ~~> (%0@(C_Cons %1 %5), H') = retrieve (H, %0)
E+R %0@(C_Cons %1 %5)                    <<H6 | . | RCase (xs%0) {qs},R5 >>
             ~~>
M %0@(C_Cons %1 %5) {C_Cons y ys -> is}  <<H6 | xs%0 | R5 >>
             ~~>
I (Jump (Eval &ys))                      <<H6 | y=%1,ys=%5,L1 | R5 >>
             ~~>
C (Eval %5)                              <<H6 | . | R5 >>
             ~~> (%5@(C_Cons %1 %4), H') = retrieve(H, %5)
E+R %5@(C_Cons %1 %4)                    <<H6 | . | Update %3,R4 >>
             ~~> (%5,H') = replace(H, %3, %5@(C_Cons %1 %4))
R %5@(C_Cons %1 %4)                      <<%3->C_Cons %1 %4,H5 | . | RCase L5 {qs},R3 >>
             ~~>
M %5@(C_Cons %1 %4) {C_Cons y ys -> is}  <<H6 | L5 | R3 >>
             ~~>
I (&z <- Store (Fu_ap2 &f &x &y)         <<H6 | y=%1,ys=%4,L5 | R3 >>
             ~~> (%6, H') = allocate (H6, Fu_ap2 %2 %1 %1)
I (&zs <- Store (F_zipWith &f &xs &ys))  <<%6->Fu_ap2 %2 %1 %1,H6 | z=%6,L7 | R3 >>
             ~~> (%7, H') = allocate (H7, Fu_zipWith %2 %5 %4)
I (Return (C_Cons z zs))                 <<%7->Fu_zipWith %2 %5 %4,H7 | zs=%7,L8 | R3 >>
             ~~>
R (C_Cons %6 %7)                         <<H8 | . | Update %4,R2 >>
             ~~> (%4,H') = replace(H, %4, (C_Cons %6 %7))
R %4@(C_Cons %6 %7)                      <<%4->C_Cons %6 %7,H7 | . | RCase (n=1,xs%4) {as},R1 >>
             ~~>
M %4@(C_Cons %6 %7) {C_Cons y ys -> is}  <<H8 | n=1,xs%4 | R1 >>
             ~~>
I (&z <- Prim (==) &n 0;is)              <<H8 | ys=%7,n=1,L2 | R1 >>
             ~~>   Res _z = runPrim (==) 3 0
I (If &z then as else bs)                <<H8 | z=F,ys=%7,n=1,L2 | R1 >>
             ~~>   is = if False then as else bs
I (&m <- Prim (-) &n 1);is)              <<H8 | z=F,ys=%7,n=1,L2 | R1 >>
             ~~>   Res _m = runPrim (-) 1 1
I (Jump (Call indexAt &m &ys))           <<H8 | m=0,z=F,ys=%7,L3 | R1 >>
             ~~>
C (Call (indexAt 0 %7))                  <<H8 | . | R1 >>
             ~~> (&n &xs, is) = lookup(indexAt)
I Case (Eval &xs) {as}                   <<H8 | n=0,xs%7 | R1 >>
             ~~>
C (Eval %7)                              <<H8 | . | RCase (n=0,xs%7) {as},R1 >>
             ~~> (%7@(Fu_zipWith %2 %5 %4), H') = retrieve (H, %7)
E %4@(Fu_zipWith %2 %5 %4)               <<H8 | . | R2 >>
             ~~> (&f &as &bs, is) = lookup(zipWith)
I (Case (Eval &as) {qs})                 <<H8 | f=%2,as=%5,bs=%4 | Update %7,R2 >>
             ~~>
C (Eval %5)                              <<H8 | . | RCase (f=%2,as=%5,bs=%4) {qs},R3 >>
             ~~> (%4@(C_Cons %1 %4)) = retrieve (H, %7)
E+R %4@(C_Cons %1 %4)                    <<H8 | . | RCase (f=%2,as=%5,bs=%4) {qs},R3 >>
             ~~>
M %4@(C_Cons %1 %4) {C_Cons x xs -> is}  <<H8 | f=%2,as=%5,bs=%4 | R3 >>
             ~~>
I (Case (Eval &bs) {qs})                 <<H8 | x=%1,xs=%4,L3 | R3 >>
             ~~>
C (Eval %4)                              <<H8 | . | RCase (x=%1,xs=%4,L3) {qs},R3 >>
             ~~> (%4@(C_Cons %6 %7)) = retrieve (H, %4)
E+R %4@(C_Cons %6 %7)                    <<H8 | . | RCase (x=%1,xs=%4,L3) {qs},R3 >>
             ~~>
M 4@(C_Cons %6 %7) {C_Cons &y &ys -> is} <<H8 | . | R3 >>
             ~~>
I (&z <- Store (Fu_ap2 &f &x &y)         <<H8 | y=%6,ys=%7,L5 | R3 >>
             ~~> (%8, H') = allocate (H8, Fu_ap2 %2 %1 %6)
I (&zs <- Store (F_zipWith &f &xs &ys))  <<%8->Fu_ap2 %2 %1 %6,H8 | z=%6,L7 | R3 >>
             ~~> (%9, H') = allocate (H9, Fu_zipWith %2 %5 %4)
I (Return (C_Cons &z &zs))                <<%9->Fu_zipWith %2 %4 %7,H9 | zs=%7,L8 | R3 >>
             ~~>
R (C_Cons %8 %9)                         <<H10 | . | Update %7,R2 >>
             ~~>  (%7, H) = replace(H, %7, (C_Cons %8 %9))
R %7@(C_Cons %8 %9)                      <<%7->C_Cons %8 %9,H9 | . | RCase (n=0,xs%7) {as},R1 >>
             ~~>
M %7@(C_Cons %8 %9) {C_Cons &y &ys-> is} <<H10 | n=0,xs%7 | R1 >>
             ~~>
I (&z <- Prim (==) &n 0;is)              <<H10 | y=%8,n=0,L2 | R1 >>
             ~~>   Res _z = runPrim (==) 0 0
I (If &z then as else bs)                <<H10 | z=T,y=%8,L3 | R1 >>
             ~~>   is = if True then as else bs
I (Jump (Eval &y)                        <<H10 | z=T,y=%8,L3 | R1 >>
             ~~>
C (Eval %8)                              <<H10 | . | R1 >>
             ~~>   (%8@(Fu_ap2 %2 %1 %6), H') = retrieve(H, %8)
E  %8@(Fu_ap2 %2 %1 %6)                  <<H10 | . | R1 >>
             ~~>
C (Eval %2, Apply %1 %6)                 <<H10 | . | Update %8,R1 >>
             ~~>
C (Eval %2)                              <<H10 | . | Apply %1 %6,R2 >>
             ~~>   (%2@P2_plus) = retrieve(H, %2)
E+R %2@P2_plus                           <<H10 | . | Apply %1 %6,R2 >>
             ~~>  (&a &b, is) = lookup(plus)
I (C_Int &x <- Call (Eval &a);is)        <<H10 | a=%1,b=%6 | R2 >>
             ~~>
C (Eval %1)                              <<H10 | . | RCase (a=%1,b=%6) {C_Int &x ->is},R2 >>
             ~~>   (%1@(C_Int 1), H') =  retrieve(H, %1)
E+R %1@(C_Int 1)                         <<H10 | . | RCase (a=%1,b=%6) {C_Int &x ->is},R2 >>

I (C_Int &y <- Call (Eval &b);is)         <<H10 | x=1,a=%1,b=%6 | R2 >>
             ~~>
C (Eval %6)                              <<H10 | . | RCase L3 {C_Int &y ->is},R2 >>
             ~~>   (%6@(Fu_ap2 %2 %1 %1), H') =  retrieve(H, %6)
E  %6@(Fu_ap2 %2 %1 %1)                  <<H10 | . | R3 >>
             ~~>
C (Eval %2, Apply %1 %1)                 <<H10 | . | Update %6,R3 >>

C (Eval %2)                              <<H10 | . | Apply %1 %1,R4 >>
             ~~>   (%2@P2_plus) = retrieve(H, %2)
E+R %2@P2_plus                           <<H10 | . | Apply %1 %1,R4 >>
             ~~>  (&a &b, is) = lookup(plus)
I (C_Int &x <- Call (Eval &a);is)        <<H10 | a=%1,b=%1 | R4 >>
             ~~>
C (Eval %1)                              <<H10 | . | RCase (a=%1,b=%1) {C_Int &x ->is},R4 >>
             ~~>   (%1@(C_Int 1), H') =  retrieve(H, %1)
E+R %1@(C_Int 1)                         <<H10 | . | RCase (a=%1,b=%1) {C_Int &x ->is},R4 >>

I (C_Int &y <- Call (Eval &b);is)         <<H10 | x=1,a=%1,b=%1 | R4 >>
             ~~>
C (Eval %1)                              <<H10 | . | RCase L3 {C_Int &y ->is},R4 >>
             ~~>   (%1@(C_Int 1), H') =  retrieve(H, %1)
E+R %1@(C_Int 1)                         <<H10 | . | RCase L3 {C_Int &y ->is},R4 >>
             ~~>
M   %1@(C_Int 1) {C_Int &y ->is}         <<H10 | x=1,a=%1,b=%1 | R4 >>
             ~~>
I (&z <- Prim (+) &x &y;is)              <<H10 | y=1,x=1,a=%1,b=%1 | R4 >>
             ~~>   Res _z = runPrim (+) 1 1
I (Return (C_Int &z))                    <<H10 | z=2,L4 | R4 >>
             ~~>
R (C_Int 2)                              <<H10 | . | Update %6,R3 >>
             ~~>  (%6, H') = replace(H10, %6, C_Int 2)
R (C_Int 2)                              <<%6->C_Int 2,H9 | . | RCase L3 {C_Int &y ->is},R2 >>
             ~~>
M %6(C_Int 2) {C_Int &y ->is}            <<H10 | x=1,a=%1,b=%6 | R2 >>
             ~~>
I (&z <- Prim (+) &x &y;is)              <<H10 | y=2,x=1,a=%1,b=%6 | R2 >>
             ~~>   Res _z = runPrim (+) 1 2
I (Return (C_Int &z))                    <<H10 | z=3,L4 | R2 >>
             ~~>
R (C_Int 3)                              <<H10 | . | Update %8,R1 >>
             ~~>  (%8, H') = replace(H10, %8, C_Int 3)
R (C_Int 3)                              <<%8->C_Int 3,H9 | . | RCase (xs=%0) {C_Int &n -> rs},R0 >>
             ~~>
M %8@(C_Int 3) {C_Int &n -> rs}          <<H10 | xs=%0 | R0 >>
             ~~>
I (Jump (IO printInt &n))                <<H10 | n=3,xs=%0 | R0 >>
             ~~>
C  (IO printInt 3)                       <<H10 | . | R0 >>
             ~~>   runIO (printInt 3)
```

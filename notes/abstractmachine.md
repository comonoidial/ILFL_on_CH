# Deriving an abstract machine for executing lazy functional languages

<!-- ### Designing an intermediate language for lazy functional languages by abstract machine derivation. -->

The big gap between how lazy functional language are executed and the capabilities in the instruction set of conventional processors makes efficient compilation a considerable challenge.
As an intermediate step in the compilation process many abstract-machines and intermediate languages have been developed.
The separation of the essential aspects of the evaluation steps and data representation from the messy concrete machine code generation led eventually to good compilation techniques for lazy functional languages.

Some of the abstract machines are mainly interesting from the theoretical point and ignore many practical and efficiency concerns.
The earlier approaches of template instantiation and combinator reduction were still far removed from traditional instruction sets, so much that specialized hardware architectures were developed.
The concept of compiled graph reduction where each function is represented as a concrete sequence of actions on the graph to be reduced, became the main implementation approach in the early 90's.
This allowed the application of conventional compiler techniques and efficient execution on available stock hardware.
Many variations on the compiled graph reduction theme exist making different design choices in: heap data representation, calling conventions, dealing with partial applications, and how to handle primitive values and operations.
Over time some of the design choices were reconsidered due to evolution of hardware, but general approach has remained the same for over 20 years.
The other differences are in the level of abstraction in the intermediate, ranging from sequences of instructions using explicit stack indices to a constrained and annotated core language with an operational semantics.
<!-- %% TODO references %% -->


##### Requirements for an intermediate language targeting custom hardware.

Instead designing an instruction set for a specialized processor from scratch, it is beneficial to first consider its requirements and semantics on an abstracter level.
The question is what are the properties of an intermediate language suitable to derive a concrete instruction set from.

 * Efficiency:
   The first property is making efficient execution possible.
   except for intellectual curiosity, the only reason to design a specialized processor is performance
   compiler optimizations/annotations needs to be expressible

 * Abstraction:
   architecture independent
   not committing to minor design choices

 * Simplicity:
   even with cheap transistors, the cost of complexity is high, especially in development and verification time

 * Completeness:
   No magic primitives that know about or manipulate the internal state of the evaluator.
   Thus unspecified primitives can only either pure (arithmetic) internal operations or fully external (io) operations.



### Deriving a practical abstract machine
We start with looking only at the actual steps that needs to taken to evaluate a program, taking the operational perspective of an abstract machine.
We view compilation as a separate problem, thus the abstract machine manipulates only expressions (unlike some others that include compilation and instruction and are better called virtual machines).
having instructions makes a machine concrete in the aspect of who is responsible for certain action, for example the caller callee divide in push/enter vs eval/apply and who does the updating
calling conventions and how variables are implemented are other aspects in which some abstract machines are concrete
while how arguments are passed to/returned from function is very important for an efficient implementation on specific processor architecture, it is conceptually an trivial thing on abstract machine level
and register/stack allocation of variables depends strongly on the available instruction set, thus using constructs like de Bruijn encoding is a premature complication, that only shows that some variable allocation method exist

<!-- %% TODO integrate these observation in introduction above -->

##### Lambda calculus
<!-- %% tell more about lambda calculus here -->

The syntax of the untyped lambda calculus:
```
E ::= x | \x->E | E E
```
<!-- %% something about manipulating lambda calculus expressions -->
beta reduction / capture free substitution

free variables

typed lambda calculus (well typed programs do not get stuck)

evaluation strategies/choosing where to apply beta reduction

weak head normal form

let expression

encoding data types

Y combinator

##### Call by name evaluation
We use call by name evaluation as a starting point, as it is simpler than call by need evaluation.
Later in the derivation process we will stumble across an optimisation that converts call by name into call by name.

(natural) semantics of call by name evaluation:
```
   \x->L => \x->L            (Lam)

 M => \x->L    L[x/N] => K
--------------------------   (App)
         M N => K
```
<!-- %% maybe show a derivation tree here -->

### Introducing the abstract machine
> *a computation in sequence of steps only takes a stack of things saved for later*

The Logical rules from natural semantics do not tell us how to evaluate an expression on a physical machine.
Reading the App rule clockwise starting from the bottom (following the dependencies) yields a plan for stepwise execution.

First we have to evaluate the function part of the application, while saving the argument expression somewhere.
Once we have a lambda expression as control expression we substitute its binder variable in lambda body with a previously saved argument expression.
As we arrive back at the saved argument expressions only after function part was finished evaluating, the arguments expressions form a stack.
<!-- need a clearer argument here for a stack -->
A minimal abstract machine for call by name evaluation:
```
    Control    Stack       Rule
----------------------------------
    M N        S           app_1
==> M          N : S

    \x->L      N : S       app_2
==> L[x/N]     S
```
Note the absence of rules for a variable as control expression and a lambda with an empty stack, the abstract machine halts on these cases with the control expression as result.
This abstract machine formulation has as an additional benefit that we can decide which step to take by just looking at the kind of control expression and the top of the stack.
<!-- , and we don't have to search through the whole expression for a place where to apply a rule.
 %% is already the case with leftmost outermost reduction of call by name??
 -->


##### Simplifying substitution
> *With an unique name an expression is remembered for later, transforming substitution into plain renaming*

While above abstract machine gives straightforward stepwise approach to evaluation, it is far from a practical implementation as most of the work is hidden in the non trivial and expensive substitution operation.

A problem with expression substitution is that it may duplicate the argument expression, which costs extra memory.
And substitution of a variable with an expression grows the control expression, thus making later substitution more expensive.

A partial solution for avoiding duplication is instead of substituting an expression, naming that expression and save it somewhere, and then substitute with the new name instead.
We will call the set of saved named expressions the heap, extending the abstract machine state with a mapping from variables to expressions.
The _app_2_ rule now needs to generate a fresh variable to ensure the name is a unique key in the heap, and use that fresh variable for substitution by renaming.
The new _var_ rule deals with heap bound variables and replaces that variable in the control with a copy its associated expression from the heap.
```
    Heap      Control          Stack       Rule
------------------------------------------------
    H         M N              S           app_1
==> H         M                N : S

    H         \x->L            N : S       app_2
==> H[p->N]   L[x/p]           S           (fresh p)

    H[p->M]   p                S           var
==> H[p->M]   M                S
```
As a bonus the control expression will not grow in any of the rules, thus will be bounded by the size of the input program.


##### Garbage collecting the heap
> *selectively forgetting things is the essence of a working memory*

One problem of this abstract machine is that the heap keeps on growing, making the evaluation process run out of memory relatively quickly.
Not all entries in the heap will be used again, but predicting exactly which will be is an intractable problem.
However we can make a safe approximation of which heap entries might used again in the future.

To find which heap entries might be reachable, we first gather all the free variables in control expression and stack.
All the heap entries associated with these variable are reachable, and also indirectly those with variables in reachable heap entries.
Then the heap entries which have not been found reachable can be removed from the heap (garbage collected).

In contrast in the first abstract machine (without the heap) keeping track of the memory used for expressions was straightforward:
The argument in the _app_1_ rule is moved from control expression on to stack.
During the substitution in the _app_2_ rule for each occurrence of the lambda variable a copy is allocated for the argument expression.
And after substitution the argument expression is removed from the stack, and its memory freed.

Thus by merely optimizing substitution we have introduced the need for a garbage collector!
Now traversing a (growing) control expression in each substitution is replaced by traversing the whole reachable heap for garbage collection.
Fortunately using spare memory we can amortize the cost of the garbage collection traversal over thousands or even millions of evaluation steps.
Thus the expression heap is still a big win from the performance point of view, however the need for a garbage collection process is a serious investment from implementation complexity point of view.
<!-- %% TODO something about reference counting here -->

We will view garbage collection as an external process that occasionally interrupts the program evaluation, to avoid complicating the abstract machine description.  

##### A stack of names instead of expressions
One glaring inefficiency of the current abstract machine is in the application rules.
The argument of an application is first copied to the stack, only to be copied again from the stack to the heap later.
To avoid this extra copying, we can move the heap allocation of the applied expression from the _app_2_ rule to the _app_1_ rule.
This yields the following abstract machine:
```
    Heap      Control          Stack       Rule
------------------------------------------------
    H         M N              S           app_1
==> H[p->N]   M                p : S      (fresh p)

    H         \x->L            p : S       app_2
==> H         L[x/p]           S

    H[p->M]   p                S           var
==> H[p->M]   M                S
```
As an additional benefit the stack is simplified to contain only simple variables instead of arbitrary expressions.

##### Let heap allocation be explicit
<!--  %% TODO show the inefficiency -->
When the argument is just a variable it is not efficient to introduce an indirection on the heap to it, however avoiding that requires a extra specialized _app_1_ rules:
```
    H         M y       S        app_1_var
==> H         M         y : S
    H         M N       S        app_1_exp
==> H[p->N]   M         p : S    (fresh p)
```
This adds the complication of having to inspect the argument of an application to determine which rule to apply.
Alternatively we could try eliminate one of both cases by preprocessing the program so that one of them cannot occur anymore.
We can try to restrict the argument of application to variable by rewriting it using a lambda:
```
M N  ~~>  (\x -> M x) N  (unique x)
```
While the argument of ```M``` is now a simple variable, we have only shifted the problem of complex arguments to the new outer application.
Thus we need another way to distinguish both kind of applications by making it explicit in the syntax.
We could annotate each complex application ```M N  ~~>  M@N```
A commonly used syntactic sugar (related to applications) in the lambda calculus is the let expression:
```
(\x -> A) B  <~~>  let x = B in A
```
The main advantage of using let expressions is the improved readability by keeping the variable ```x``` close to its bound expression ```B``` independently of how large ```A``` is.

Using let expressions we can now rewrite complex applications into simple variable applications:
```
M N  ~~>  (\x -> M x) N   ~~>  let x = N in M x  (unique x)
```
Enforcing simple variable applications and adding lets changes the syntax of expressions:
```
E ::= x | \x->E | E x | let x = E in E
```
<!-- %% TODO explain let rule here and above text is probably too concise -->
The rule for let is derived from its desugared form, which is the _app_1_ rule followed by the _app_2_ rule merged into one:
```
    H         let z = M in B   S           let
==> H[p->M]   B[z/p]           S           (fresh p)
```
It allocates its binder on the heap and substitutes the variable in the control expression at the same time.
Secondly the combination of rules avoids using the stack.
One minor drawback of the let rule is that we now have variable substitution in a second place in the abstract machine, but that is problem for later.

The new let rule and the simplified _app_1_ rule yield the following abstract machine:
```
    Heap      Control          Stack       Rule
------------------------------------------------
    H         M y              S           app_1
==> H         M                y : S

    H         \x->L            y : S       app_2
==> H         L[x/y]           S

    H[p->M]   p                S           var
==> H[p->M]   M                S

    H         let z = M in B   S           let
==> H[p->M]   B[z/p]           S           (fresh p)
```
The nice aspect of this abstract machine is that every rule either uses the heap or modify the stack but never both.
Making heap allocation orthogonal to the concept of application is the main reason to choose for adding let expression instead of annotated applications.

##### Administrative normal form
<!-- %% TODO what is the relation of language E with ANF?? -->


##### Sharing results for lazy evaluation
> *being lazy is evaluating every heap bound expression at most once*

When a variable is used multiple times, we evaluate the same associated expression from the heap multiple times, this can be a large waste of work.
This work duplication can be avoided by splitting the _var_ rule, so that it first records on the stack which heap entry is currently being evaluated.
And the second half of the _var_ rule becomes updating a heap binding with it result control expression when it's fully evaluated.
Now we need to distinguish between application arguments ```y^A``` and update markers ```p^U``` on the stack.
A fully evaluated expression for updating can only be a lambda expression with no arguments left to apply on top of the stack.
<!-- %% FIXME what about the free variable case?? -->
```
    Heap         Control          Stack       Rule
---------------------------------------------------
    H            M y              S           app_1
==> H            M                y^A : S

    H            \x->L            y^A : S     app_2
==> H            L[x/y]           S

    H[p->M]      p                S           var_1
==> H            M                p^U : S

    H            \x->L            p^U : S     var_2
==> H[p->\x->L]  \x->L            S

    H            let z = M in B   S           let
==> H[p->M]      B[z/p]           S           (fresh p)
```
Above abstract machine is identical to the Sestoft's mark 1 machine derived from Launchbury's natural semantics for call by need evaluation.
We could have started with this abstract machine for lazy evaluation, but observing a step by step derivation of abstract machine operations gives more insight.
That starting from a minimalistic call by name abstract machine and applying operational optimisations leads us along well known semantics and machines is both an interesting convergence and reassurance that the chosen approach works.

##### Considering eliminating substitution

Substitution of variables in the _app_2_ and _let_ rules is now the most complex operation in the abstract machine.
By adding another indirection, the environment that maps variables to heap references, we can eliminate substitution entirely.

The Sestoft mark 2 machine:
```
    Heap            Control      Environment Stack       Rule
---------------------------------------------------
    H               M y            E[y->p]   S           app_1
==> H               M              E[y->p]   p^A : S

    H               \x->L          E         p^A : S     app_2
==> H               L              E[x->p]   S

    H[p->(L,E')]    x              E[x->p]   S           var_1
==> H               L              E'        p^U : S

    H               \x->L          E         p^U : S     var_2
==> H[p->(\x->L,E)] \x->L          E         S

    H               let z=L in B   E         S           let
==> H[p->(L,E)]     B              E[z->p]   S           (fresh p)
```
<!-- %%TODO explain this thing and talk about environment trimming -->

We chose to delay the introduction of the environment in our derivation, because it adds lots bookkeeping complexity obscuring upcoming optimizations to the abstract machine.
From here on of we deviate from Sestoft's abstract machine and the family of abstract machines called environment machines.


##### Inspecting heap bindings
> *bound lambdas need no updating*

Every time the abstract machine comes across a variable referring to a lambda on heap we observe the following sequence of steps:
```
    H[p->\x->L]   p       S
==> H             \x->L   p^U : S
==> H[p->\x->L]   \x->L   S
```
This is not efficient as we first take the lambda from the heap only to put a copy back, and create a temporary update marker.
At the cost of first inspecting the heap binding we can combine these two step into a specialized _var_1_ rule:
```
    H[p->\x->L]   p       S
==> H[p->\x->L]   \x->L   S
```
We can apply the idea of inspecting the heap binding to other rules involving lambdas, for example rule _app_2_ specialized to a lambda bound variable becomes:
```
    H[p->\x->L]  p       y^A : S
==> H[p->\x->L]  L[x/y]  S
```
This combines the copying from the heap of the lambda body with argument substitution into one traversal of the expression tree of the body.
this is a key aspect of template instantiation
<!-- %%explain with ref -->

The straightforward variation of the _var_2_ rule with heap inspection would be the following:
```
    H[y->\x->L]           y   p^U : S
==> H[y->\x->L,p->\x->L]  y   S
```
However copying the lambda expression is a waste of memory, and this be avoided by letting p refer to the existing lambda instead:
```
    H[y->\x->L]       y   p^U : S
==> H[y->\x->L,p->y]  y   S
```
When the variable p is used again later we get an extra machine step following the indirection through p, but we accept this tradeoff for saving memory.
Going through updating steps for simple variable indirections is wasteful, thus we add a variation of the var_1 rule in the same way as we did for lambdas:
```
    H[p->y]   p   S
==> H[p->y]   y   S
```
Now we have specialized versions of all rules involving lambdas, we never need to copy a heap bound lambda to the control expression anymore.
Thus instead of a lambda specialized var_1 rule we can add a side condition excluding lambdas:
```
    H[p->M]      p                S       where M is not a lambda
==> H            M                p^U :S
```
This will halt the abstract machine on encountering an empty stack and a variable bound with lambda, extending the definition of an evaluated expression.


##### Restricting where lambdas may occur

While we have optimized how we deal with heap bound lambdas, lambdas can still occur as the control expression.
This makes the abstract machine more complex by two variations of all rules dealing with lambdas.
And the updating in the _var_2_ rule can copy a lambda from a control expression to the heap.
<!-- TODO research why bound lambdas in STG, cite ricardo jfp09 -->
<!-- restricting lambdas to lets only avoids copying them from/to heap and happens to enable further optimisations  -->

A solution is to make sure we have only to deal with lambdas on the heap, by restricting the syntax of the language again by only allow lambdas in let bound expressions:
```
E ::= x | E y | let x = B in E
B ::= \x->E | E
```
Using this restricted language and applying the heap inspecting optimization from previous section, we arrive at the following abstract machine:
```
    Heap             Control        Stack       Rule
------------------------------------------------------
    H                M y            S           app_1
==> H                M              y^A : S

    H[p->\x->L]      p              y^A : S     app_2
==> H[p->\x->L]      L[x/y]         S

    H[p->y]          p              S           var_1a
==> H[p->y]          y              S

    H[p->M]          p              S           var_1b
==> H                M              p^U :S    (M is not a lambda)

    H[p->\x->L]      p              q^U : S     var_2
==> H[p->\x->L,q->p] p              S

    H                let z=M in B   S           let
==> H[p->M]          B[z/p]         S           (fresh p)
```
<!-- %%TODO relation to observations in Mountjoy's paper (section 3) -->


##### Preprocessing the program for faster evaluation

We have now changed the syntax of our source expressions twice to make the abstract machine more efficient or less complex.
Moving work and complexity from the evaluator to the compiler seems a good idea in general.
Thus the question arises what other helpful transformations could be done by a compiler.

Applications on explicit lambdas can be transformed into a let expression at compile time:
```
(\x -> E) A  ~~>  let x = A in E
```
Trivial let expression can be removed by substituting variables:
```
let x = y in E  ~~>  E[x/y]
```
When a let bound expressions is used once we can eliminate the let by substitution.
Without the single use requirement substituting let expression could loose sharing, however a compiler may decide to duplicate expressions if they are small enough.  
Also unused let expressions can be removed.
```
let x = B in E  ~~>  E[x/B]  (where x is used at most once in E)
```

Nested let expressions can be flattened to a chain of let expressions:
```
let x = (let y = B in M) in E  ~~>  let y = B in let x = M in E
```
The performance effects of this transformation depend on the circumstances and details of a specific implementation.
If M is a value then we can save the update steps on the variable x, but when x is not used y has been allocated unnecessarily.
in general bulk allocation of a chain let expression is fairly cheap
with nested let expressions the steps taken on first use of x variable, depend on loading x binding from the heap and then executing the corresponding expression
the cost of this is the latency of accessing the heap followed by the latency of starting the execution of a new expression
<!-- %% FIXME above is not quite correct (depends on situation), cost of heap access is always there, and passing in the free variables of B also has some cost -->
Thus we have a tradeoff depending on the percentage of use of the outer let binding and the size of the nested let expression.
The cost tradeoff between extra allocations or extra incurred latency depends a lot on the target implementation/processor architecture.
For now we choose to always flatten the lets, and leave the alternative choice as potential optimization for the code generator for a specific target.

<!-- %%reference to Santos let floating -->
Let expressions can be floated out of applications:
```
(let x = B in E) y ~~> let x = B in E y
```
This has in itself no performance effect, as it only switches the order of heap allocation and stack pushing.
However this transformation can expose further transformations, and makes chains of applications visible which we will exploit later.

The restricted expression syntax with simple subexpressions becomes:
```
E ::= S | let x = B in E
S ::= x | S y
B ::= \x->E | S y
```
This syntax enforces that above transformations (except for eliminating non trivial lets) have been applied.
The abstract machine itself requires no changes to support this restricted syntax, thus we will not repeat it here.

We could restrict the function part of an application to a variable ```let f = S in f y```,  however that costs a lot of extra abstract machine steps and does not simplify anything.

The remaining potential transformations involve the interaction between lambdas and let expressions.
In principle let expressions could be floated into lambdas as following:
```
let x = B in \y -> E  ~~>  \y -> let x = B in E
```
However this has no benefits and cause B to duplicated for each time the lambda is applied, thus we will not use this transformation.

The other way around of floating a let out of a lambda is possible when the let bound expression does not depend on the lambda variable:
```
\x -> let y = B in E  ~~>  let y = B in \x -> E  (where B does not contain x)
```
This transformation is better known as the full laziness transformation.

it helps matching the programmers intuition of evaluating a constant expression only once
<!-- %%TODO more about full laziness and Santos -->

##### Supporting multiple arguments in applications and lambdas
>  *handling multiple arguments at once takes less work*

Lambdas and applications are in practice often nested, which make the abstract machine goes through series of the same application rules.
By combining series of identical abstract machine steps, we allow implementations to exploit low level parallelism and have less control overhead with the reduced number of steps.
Therefore we will generalize the source language by changing lambdas and applications from single variables to having multiple parameters/arguments.
Also this solves a problem where the previous introduced restriction on lambda expressions caused a chain of single parameter lambdas to become a quite convoluted expression using extra lets and variables.

Supporting multiple arguments is only a minor extension to the language, where each application and lambda has a vector of variables:
```
E ::= S | let x = B in E
S ::= x | S y_n
B ::= \x_n->E | S y_n
```
The _app_1_ and _var_2_ rules need both a small syntactic update, and the _var_1b_ rule now only handles applications.
For the _app_2_ rule we hit some complications, because we can now have too few or too many arguments available on the stack.
In case of having a matching number of arguments the rule is essentially the same as before:
```
    H[p->\x_n->L]      p              y_n^A : S
==> H[p->\x_n->L]      L[x_n/y_n]     S
```
With too many arguments on the stack we leave the surplus on the stack, using the same rule reinterpreting ```y_n^A``` as the first n arguments on the stack.
And with not enough arguments on stack we could do a partial substitution of the lambda expression, but that results in lambda as control expression which we avoided earlier.
Analysing the causes of not enough arguments on stack gives some ideas for an alternative approach.
In case the stack is empty, which means we are done evaluating, we can handle this case by extending the definition of the final state.
The only other possible case is that an argument chain is interrupted by an update marker.
This gives us the option of making the partial substitution of the lambda expression a new heap binding while dropping the update marker:
```
H[p->\x_m->L]                 p   y_n^A : q^U : S   (m > n)
==> H[p->\x_m->L,
      q-> \x_k->L[x_n/y_n]]   p   S                 (k = m-n)
```
However this introduces the complex substitution operation in a new place in the abstract machine.
A better solution is to make the new heap binding a partial application of the lambda reference with the currently available arguments:
```
H[p->\x_m->L]       p   y_n^A : q^U : S   (m > n)
==> H[p->\x_m->L,
      q-> p y_n]    p   y_n^A : S
```
Having to look deeper into stack is not ideal, however we will accept this minor complication for now.

All these changes combined yield the following abstract machine:
```
    Heap               Control        Stack       Rule
--------------------------------------------------------
    H                  M y_n          S           app_1
==> H                  M              y_n^A : S

    H[p->\x_n->L]      p              y_n^A : S   app_2a
==> H[p->\x_n->L]      L[x_n/y_n]     S

    H[p->\x_m->L]      p              y_n^A : q^U : S  
==> H[p->\x_m->L,                        (m > n)  app_2b
      q-> p y_n]       p              y_n^A : S

    H[p->y]            p              S           var_1a
==> H[p->y]            y              S

    H[p->M y_n]        p              S           var_1b
==> H                  M y_n          p^U :S

    H[p->\x_n->L]      p              q^U : S     var_2
==> H[p->\x_n->L,q->p] p              S

    H                  let z=M in B   S           let
==> H[p->M]            B[z/p]         S           (fresh p)
```
<!-- %%TODO relation to observations in Mountjoy's paper (2) multiple arguments  also app_2b 'squeeze' operation stg / mountjoy sect. 6 -->

##### Making free variables explicit
> *let lambdas bind their free variables*

Inspecting the heap of non-trivial programs one will observe that many lambdas look very similar.
The lambdas on heap originating from the same let binding in the program vary only in the usage of a few variables.
Only the free variables within lambdas can differ between instances on the heap, as they might have been substituted in the app or let rules.

We can extract the changing part from lambda by introducing a extra lambda parameter for each free variable, and replace the original lambda binding with a application of the new lambda with the free variables.
For example:
```
let f = \x y -> a ... y ... x ... b in ..
```
Where a and b are the free variables of the lambda expression, transforms into:
```
let f' = \a' b' x y -> a' ... y ... x ... b' in
let f = f' a b in ...
```
<!-- %% TODO something about optimizing code if the new f is used in an application -->
Now that lambda have no free variables anymore the substitution operation can skip traversing lambdas.
<!-- %% TODO consider moving language changes to code/data split step -->
And because lambda expression have no dependencies anymore, they can be floated out as toplevel definitions of the program.
We can express this in the following specialization of the language:
```
P ::= def f = \x_n -> E in P | E
E ::= S | let x = B in E
S ::= x | f | S y_n
B ::= S y_n
```
```
    H                 def f=L in P   S           fun
==> H[f->L]           P              S
```
<!-- %% TODO lambda lifting vs STG closures (better in subsection at the end??) -->


##### Annotation of partial applications
> *and partial applications count their deficit*

The downside of transforming lambdas using an extra application for the free variables is that we can not see directly from an application on the heap whether it is effectively partial applied lambda.
This can cause extra updates steps, which we tried to avoid earlier.
A solution for this problem is to annotate whether an application is partial by including the number of missing arguments:
```
P ::= def f = \x_n -> E in P | E
E ::= x | x y_n | let x = B in E
B ::= x-k y_n
```
We only allow a non zero missing arguments annotation if the function variable is known to be an lambda.

Both the _app_2a_ and _app2_b_ rules need an extra variant to deal with partial application of lambdas.
Partial applications are evaluated values thus need no update machinery.
This means that the _var_1b_ rule can be restricted to non partial applications.
And the _var_2_ rule needs an extra variation for creating indirections to partial applications.

```
    Heap               Control        Stack       Rule
--------------------------------------------------------
    H                  x y_n          S           app_1
==> H                  x              y_n^A : S

    H[p->\x_n->L]      p              y_n^A : S   app_2a
==> H[p->\x_n->L]      L[x_n/y_n]     S

    H[p->\x_m->L]      p              y_n^A : q^U : S  
==> H[p->\x_m->L,                        (m > n)  app_2b
      q->p-k y_n]      p              y_n^A : S   (k = m-n)

    H[f->\x_m->L,      p              y_n^A : S
      p->f-n z_l]                         (n > 0) app_3a
==> H[f->\x_m->L,      L[x_m/z_l+y_n] S
      p->f-n z_l]  

    H[p->f-m x_l]      p              y_n^A : q^U : S  
==> H[p->f-m x_l,                        (m > n)  app_3b    
      q->f-k x_l+y_n]  p              y_n^A : S   (k = m-n)

    H[p->y]            p              S           var_1a
==> H[p->y]            y              S

    H[p->x y_n]        p              S           var_1b
==> H                  x y_n          p^U : S

    H[p->\x_n->L]      p              q^U : S     var_2a
==> H[p->\x_n->L,q->p] p              S

    H[p->f-k x_n]      p              q^U : S     var_2b
==> H[p->f-k x_n,q->p] p          S

    H                  let z=M in B   S           let
==> H[p->M]            B[z/p]         S           (fresh p)

    H                  def f=L in P   S           fun
==> H[f->L]            P              S
```

##### Splitting lambdas from applications, separating 'code' from 'data'
> *lambdas have written the rules of their evaluation, and let applications gather all information*


splitting the heap into application and lambdas
now we need both variable application and (potential partial) known function application
only fully applied functions are allowed as expression to avoid duplicating _app_3a/b_ rule on partial application as control expression
```
P ::= def f = \x_n -> E in P | E
E ::= f-0 y_n | x y_n | x | let x = B in E
B ::= f-k y_n | x y_n
```
<!-- %% FIXME what about over application of known functions? -->
```
    Globals        Heap               Control        Stack       Rule
------------------------------------------------------------------------
    G[f->\x_n->L]  H                  f-0 y_n        S           call
==> G[f->\x_n->L]  H                  L[x_n/y_n]     S

    G              H                  x y_n          S           app_1
==> G              H                  x              y_n^A : S

    G[f->\x_m->L]  H[p->f-n z_l]      p              y_n^A : S   app_2a
==> G[f->\x_m->L]  H[p->f-n z_l]      L[x_m/z_l+y_n] S

    G              H[p->f-m x_y]      p              y_n^A : q^U : S  
==> G              H[p->f-m x_y,                        (m > n)  app_2b    
                     q->f-k x_y+y_n]  p              y_n^A : S   (k = m-n)

    G              H[p->y]            p              S           var_1a
==> G              H[p->y]            y              S

    G[f->\x_n->L]  H[p->f-0 y_n]      p              S           var_1b
==> G[f->\x_n->L]  H                  L[x_n/y_n]     p^U : S

    G              H[p->x y_n]        p              S           var_1c
==> G              H                  x y_n          p^U : S

    G              H[p->f-k x_n]      p              q^U : S     var_2
==> G              H[p->f-k x_n,q->p] p              S

    G              H                  let z=M in B   S           let
==> G              H[p->M]            B[z/p]         S           (fresh p)

    G              H                  def f=L in P   S           fun
==> G[f->L]        H                  P              S
```

##### Adding heap references to control expressions
> *summoned expressions remember their bounds*

Accessing heap memory is not cheap in time and energy, thus we want to avoid accessing the same heap location in subsequent rules.
And because of memory latency it has no practical benefit to lookup something on the heap and manipulate the retrieved data in the same rule.
For some rules we need the corresponding heap location of an expression, thus we extend the control expression to optionally include a heap reference.

Separating heap access from using the retrieved expression requires a large amount of small changes in the abstract machine.
the _var_ rules are split into a heap access and update rules
rule _app_2b_ is greatly simplified by extraction of the update aspect to _update_2b_
and a few rules now account for an optional heap reference denoted with parentheses (```(r->)M```) , even if they do not use it
as small optimization _app_1_ now includes fetching its function reference from the heap
<!-- %% FIXME is the app_1 optimisation useful at all?? -->
```
    Globals        Heap           Control        Stack       Rule
-------------------------------------------------------------------
    G              H[p->M]       (r->)p          S           var
==> G              H[p->M]        p->M           S

    G[f->\x_n->L]  H              p->f-0 y_n     S           update_1a
==> G[f->\x_n->L]  H[p->Hole]     L[x_n/y_n]     p^U : S

    G              H              p->x y_n       S           update_1b
==> G              H[p->Hole]     x y_n          p^U : S

    G              H              p->f-k x_n     q^U : S     update_2a
==> G              H[q->p]        p->f-k x_n     S

    G              H              f-k x_n        q^U : S     update_2b
==> G              H[q->f-k x_n]  q->f-k x_n     S

    G[f->\x_n->L]  H              f-0 y_n        S           call
==> G[f->\x_n->L]  H              L[x_n/y_n]     S

    G              H[x->M]        x y_n          S           app_1
==> G              H[x->M]        x->M           y_n^A : S

    G[f->\x_m->L]  H             (p->)f-n z_l    y_n^A : S   app_2a
==> G[f->\x_m->L]  H              L[x_m/z_l+y_n] S

    G              H             (p->)f-m x_l    y_n^A : S   app_2b
==> G              H              f-k x_l+y_n    S    (m > n, k = m-n)

    G              H              let z=M in B   S           let
==> G              H[p->M]        B[z/p]         S           (fresh p)

    G              H              def f=L in P   S           fun
==> G[f->L]        H              P              S
```
Also with this abstract machine we don't need the restriction on function applications in expressions anymore:
<!-- FIXME explain -->
```
P ::= def f = \x_n -> E in P | E
E ::= f-k y_n | x y_n | x | let x = B in E
B ::= f-k y_n | x y_n
```

##### Using a local environment instead of substitution
> *no local variable will substituted as they are bound immediately*

Now we will eliminate substitution (as in the Sestoft mark 2 machine) by introduction of a local environment mapping variables to heap reference.
For clarity the set of used names distinguishes heap references (p,q,r) from local variables (x,y,z).
Because free variables have been eliminated from lambdas previously and the heap only contains application like expressions, we do not have to store an environment with each heap binding.
<!-- FIXME above is not a good explanation -->
this also means that all heap bindings will contain only variables that are references to heap bindings
all environments are local to the current expression being evaluated, which simplifies practical implementation of this abstract machine
also environment trimming is not required

Some rules have to be duplicated to deal with the fact that an expressions loaded from the heap contain only references, and expressions from the source program contain only environment bound local variables.
For example the _var_ rule yields two variations _var_local_ and _var_ref_:
```
    G              H[p->M]        x                E[x->p]          S           var_local
==> G              H[p->M]        p->M             _                S

    G              H[p->M]        r->p             _                S           var_ref
==> G              H[p->M]        p->M             _                S
```
We denote an empty environment by an underscore ( _ ).
This way of organizing the rules cause too much duplication of functionality, especially for function application which have three rules (_call_, _app_2a_ and _app_2b_) already.
Instead we can factor out only the environment lookup as follows:
```
    G              H              x                E[x->p]          S           exp_var
==> G              H              p                _                S

    G              H[p->M]       (r->)p            _                S           var
==> G              H[p->M]        p->M             _                S
```
This looks less efficient because every _exp_var_ rule is directly followed by a _var_ rule, however retrieving the heap bound expression depends on having looked up the variable in the environment first, so it is a sequential two step process anyway.
Any concrete implementations of this abstract machine could use fused versions of rules in case that it is more efficient to execute.

To avoid duplicating the _let_ rule for different expressions, we us the notation ```M[x]``` where _x_ stands for every variable in _M_:
```
    G              H              let z=M[x] in B  E[x->r]          S           let
==> G              H[p->M[r]]     B                E[x->r,z->p]     S           (fresh p)
```
When a function is fully applied a new environment is created with a mapping from parameters of the lambda to all applied arguments, for example in _app_2a_ rule:
```
    G[f->\x_m->L]  H             (p->)f-n q_l      _                r_n^A : S   app_2a
==> G[f->\x_m->L]  H              L                [x_m->q_l+r_n]   S
```
The complete abstract machine with a local environment is now as follows:
```
    Globals        Heap           Control          Environment      Stack       Rule
---------------------------------------------------------------------------------------
    G              H              def f=L in P     E                S           fun
==> G[f->L]        H              P                E                S

    G              H              let z=M[x] in B  E[x->r]          S           let
==> G              H[p->M[r]]     B                E[z->p,x->r]     S           (fresh p)

    G              H              x                E[x->p]          S           exp_var
==> G              H              p                _                S

    G              H              f-k y_n          E[y_n->r_n]      S           exp_fun
==> G              H              f-k r_n          _                S

    G              H              x y_n            E[x->p,y_n->r_n] S           exp_app
==> G              H              p r_n            _                S

    G              H[p->M]       (r->)p            _                S           var
==> G              H[p->M]        p->M             _                S

    G[f->\x_n->L]  H              p->f-0 r_n       _                S           update_1a
==> G              H[p->Hole]     L               [x_n->r_n]        p^U : S

    G              H              p->q r_n         _                S           update_1b
==> G              H[p->Hole]     q r_n            _                p^U : S

    G              H              p->f-k r_n       _                q^U : S     update_2a
==> G              H[q->p]        p->f-k r_n       _                S

    G              H              f-k r_n          _                q^U : S     update_2b
==> G              H[q->f-k r_n]  q->f-k r_n       _                S

    G[f->\x_n->L]  H              f-0 r_n          _                S           call
==> G              H              L                [x_n->r_n]       S

    G              H[p->M]        p r_n            _                S           app_1
==> G              H[p->M]        p->M             _                r_n^A : S

    G[f->\x_m->L]  H             (p->)f-n q_l      _                r_n^A : S   app_2a
==> G[f->\x_m->L]  H              L                [x_m->q_l+r_n]   S

    G              H             (p->)f-m q_l      _                r_n^A : S   app_2b
==> G              H              f-k q_l+r_n      _                S    (m > n, k = m-n)
```
This concludes our derivation of an efficient abstract machine for the lambda calculus.
All rules now consists of only simple operations on each element of abstract machine.
Further optimisations are possible, however they amount to adding special cases where certain rules are merged.
It is not clear-cut whether adding special cases yields enough performance gains to justify the additional complexity.
We will leave this tradeoff to concrete implementations of this abstract machine.  

### Extending the abstract machine for practical lazy functional languages

So far we focussed our efforts on the call by need implementation of the pure lambda calculus, albeit with some extensions produced by preprocessing.
a practical lazy functional language like Haskell has a very rich syntax
many of Haskell source features are just syntactic sugar and/or eliminated during compilation such as type classes
other features like algebraic datatypes and arithmetic operations could be in principle be translated into lambda calculus, but that is undesirable for efficiency
also many applications require communication with the world outside the program and/or need to handle exceptions, both interact with evaluation process

##### Algebraic datatype manipulation
```
data These a b = This a | That b | Both a b

case t of {
  This a -> ... a
  That b -> ... b
  Both a b -> ... a ... b
}

this = \a -> \fa fb fc -> fa a
that = \b -> \fa fb fc -> fb b
both = \a b -> \fa fb fc -> fc a b

t (\a -> ... a) (\b -> ... b) (\a b -> ... a ... b)

let fa = \a -> ... a
let fb = \b -> ... b
let fc = \a b -> ... a ... b
t fa fb fc
```
why not encoding datatypes as functions (reference to reduceron, SAPL)
<!-- %% TODO example with mogensen-scott encoding and explain why it is less efficient -->
```
    Globals        Heap           Control          Environment       Stack       Rule
----------------------------------------------------------------------------------------
    G              H              C y_n            E[y_n->r_n]       S           exp_con
==> G              H              C r_n            _                 S

    G              H              p->C r_n         _                 q^U : S     update_2c
==> G              H[q->p]        p->C r_n                           S

    G              H              C r_n            _                 q^U : S     update_2d
==> G              H[q->C r_n]    q->C r_n         _                 S


    G              H              case x of as     E[x->p]           S           case_1
==> G              H              p                _                 (as^M,E') : S   (E' = E|live(as))

    G              H             (p->)C q_m        _                 (as^M,E') : S   case_2a
==> G              H              A                E'[y_n->q_m]      S   (C y_n->A in as)
```
environment trimming needed to avoid space leaks
Instead of copying the local environment over to the stack in case_1 and back in case_2, we could have left the environment in place by viewing the environment as a stack of local environments.
This perspective makes a lot of sense for a practical implementation, and only reason not use it here is having to adapt all other rules to the stacked environments.

not supporting case of case because that would require copying the environment and make a environment as stack implementation harder
case of case translated away by using join point functions as in GHC

<!-- %% TODO does restricting case argument to variables make any operation sense here? maybe leave that to ALFIN, environment stack is an argument for this -->
<!-- ricardo jfp09 observes that restricting the function part of applications avoid complications with environment handling/trimming, similar argument for case exps -->

as patterns
default pattern, named default
```
P ::= def f = \x_n -> E in P | E
E ::= f-k y_n | x y_n | C y_n | x | let x = B in E | fix f-1 y_n | case x of {A+}
A ::= C y_n -> E | z@(C y_n) -> E | z -> E
B ::= f-k y_n | x y_n | C y_n
```


```
    Globals        Heap           Control          Environment       Stack       Rule
----------------------------------------------------------------------------------------
    G              H              p->C q_m         _                 (as^M,E') : S   case_2b
==> G              H              A                E'[z->p,y_n->q_m] S   (z@(C y_n)->A in as)

    G              H              C q_m            _                 (as^M,E') : S   case_2c
==> G              H[p->C q_m]    A                E'[z->p,y_n->q_m] S    (z@(C y_n)->A in as, fresh p)

    G              H              p->C q_m         _                 (as^M,E') : S   case_2d
==> G              H              A                E'[z->p]          S             (z->A in as)

    G              H              C q_m            _                 (as^M,E') : S   case_2e
==> G              H[p->C q_m]    A                E'[z->p]          S             (z->A in as, fresh p)
```

##### Throwing and catching exceptions
exceptions interact directly with the normal evaluation of a program, thus can not be implemented externally
```
E ::= ... | throw x | try e catch h
```
```
    Globals        Heap           Control         Environment  Stack       Rule
----------------------------------------------------------------------------------------
    G              H              try X catch h   E[h->q]      S           try
==> G              H              X               E[h->q]      q^C : S    

    G              H              throw x         E[x->p]      S           throw
==> G              H              Except p        _            S

    G              H[q->M]        Except p        _            q^C : S     catch_1
==> G              H              q->M            _            p^A : S

    G              H              (q->)M          _            q^C : S     catch_2
==> G              H              (q->)M          _            S

    G              H              Except p        _            X : S       unwind
==> G              H              Except p        _            S
```

##### Arithmetic and in/output operation
<!-- %% TODO combine with IO into external operations?? -->
```
  x <- IO_op y_n
  x# <- Prim_op y#_n
  let x = n
```  
<!-- %% TODO skip distinction refs/prim for alfin?? -->
<!-- %% TODO what about exception throwing ones?? -->
```
    G              H              let x = n in B   E               S         const
==> G              H              B                E[x->n]         S

    G              H              x <- e y_n; B    E[y_n->i_n]     S         external
==> G              H'             B               E[y_n->i_n,x->o] S   (o,H' = run e i_n H)
```

### Extensions for efficient lazy functional languages
memory consumption is a big concern for lazy functional languages
thus the abstract machine needs to be careful not to waste memory
and sometimes the programmer wants more control over evaluation order

##### Supporting value recursion
fixpoint function without sharing: ```fix f = f (fix f)```
lazy fixpoint function: ```fix f = letrec x = f x in x```
why fixpoint and not letrec
```
P ::= def f = \x_n -> E in P | E
E ::= f-k y_n | x y_n | x | let x = B in E | fix f-1 y_n
B ::= f-k y_n | x y_n
```
```
    G              H              fix f-1 y_n   E[y_n->r_n]  S           fix
==> G              H[p->Hole]     f-0 r_n+p     _            p^U : S     (fresh p)
```
what about a fixpoint in a lazy binder???

##### Constant applicative forms
globals/CAFs
```
    G              H              caf r = M in P   E               S         caf_def
==> G              H[r->M]        P                E               S

    G              H              let x = &p in B  E               S         caf_ref
==> G              H              B                E[x->p]         S
```

##### Selectors for avoiding space leaks
Wadler's "Fixing some space leaks with a garbage collector"

```
    G              H              x pi n           E[x->p]    S          exp_sel
==> G              H              p pi n           _          S

    G              H[p->M]        p pi n           _          S          sel_1
==> G              H[p->M]        p->M             _          n^P : S

    G              H             (p->)C q_m        _          n^P : S    sel_2
==> G              H              q!n              _          S
```

##### Update avoidance
The machinery of updating to preserve sharing is one of larger sources of overhead in lazy evaluation.
If an expression is only used once then updating is not necessary.
A compiler might be able to determine which expressions are single use only, thus we need a way exploit that information in the abstract machine.
The STG machine used in GHC has update flags on all binders.
We add update flags to function applications and specialize the update rules to deal with them:
```
    G[f->\x_n->L]  H              p->f^u-0 r_n    _          S           update_1a
==> G              H[p->Hole]     L               [x_n->r_n] p^U : S

    G[f->\x_n->L]  H              (p->)f^n-0 r_n  _          S           update_1c
==> G              H              L               [x_n->r_n] S
```
<!-- TODO say something about differences between places update flags on binders vs applications -->
a secondary use of the no-update flag is to emulate call by name
<!-- FIXME what about variable applications?? -->

##### Supporting explicit strictness
For programs in lazy languages it is occasionally important to evaluate something strictly.
Evaluating something early may reduce the memory consumption of a program, and can help the compiler in finding more optimisation opportunities.
In a language like Haskell strict evaluation can be expressed by either strictness annotations or special functions like ```seq``` and ```$!```.
Strict application could be implemented with the help of a case expression:
```
f $! x = case x of {y -> f y}
```
When x has function type its evaluation results in a partial application, however the current case rules only work for data constructors.
Thus we add two extra case rules (with and without updating) to handle strict evaluation of partial applications:  
```
    G              H              p->f-k q_m       _               (as^M,E') : S   case_3a
==> G              H              A                E'[z->p]        S             (z->A in as)

    G              H              f-k q_m          _               (as^M,E') : S   case_3b
==> G              H[p->f-k q_m]  A                E'[z->p]        S             (z->A in as, fresh p)
```


### Summary of the full abstract machine derivation

1.  Starting with abstract machine with a stack
2.  Simplifying substitution with a heap
3.  A stack of names instead of expressions
4.  Let heap allocation be explicit
5.  Lazy evaluation by updating heap bound expressions
6.  Inspecting heap binding for less updating
7.  Restricting where lambdas may occur
8.  Preprocessing the program for faster evaluation
9.  Supporting multiple arguments in applications and lambdas
10. Making free variables explicit in lambdas
11. Annotation of partial applications to recover whnf value detection
12. Separating lambdas from applications thus 'code' from 'data'
13. Adding heap references to control expressions
14. Using a local environment instead of substitution

many of these steps could have done in a slightly different order yielding the same abstract machine eventually
the chosen order of steps is an attempt to keep intermediate abstract machines as simple as possible and to make the next step manifest

key steps 1,2,4,5,9,10,14
simplifications 7,8,12,13
obvious optimisations 3,6,11

* Algebraic datatype manipulation
* Throwing and catching exceptions
* Arithmetic primitives and In/output operations
* Value recursion
* Constant applicative form
* Selectors
* Update flags
* Explicit strictness

### Comparison of design choices with other lazy FP implementations


##### Lambda lifting and representing free variables
approaches for dealing with free variables:
* saving environments on the heap
* lambda lifting, removing all free variables
* explicit free variable in binders (STG)


##### Tail calls and join points
<!-- %% TODO move to alfin chapter? -->
let no escape in STG
separate continuation stack allows for more tail calls

##### Conclusions
focusing on operational aspects only leads straightforward derivation of an efficient implementation for lazy functional languages
only a few design choices to make
graph reduction is a historic detour

### The complete abstract machine
<!-- %% maybe this is something for an appendix? -->
```
    Globals        Heap           Control          Environment      Stack       Rule
---------------------------------------------------------------------------------------
    G              H              def f=L in P     E                S           fun
==> G[f->L]        H              P                E                S

    G              H              caf r = M in P   E                S           caf_def
==> G              H[r->M]        P                E                S

    G              H              let x = &p in B  E                S           caf_ref
==> G              H              B                E[x->p]          S

    G              H              let z=M[x] in B  E[x->r]          S           let
==> G              H[p->M[r]]     B                E[z->p,x->r]     S           (fresh p)

    G              H              x                E[x->p]          S           exp_var
==> G              H              p                                 S

    G              H              f-k y_n          E[y_n->r_n]      S           exp_fun
==> G              H              f-k r_n          _                S

    G              H              x y_n            E[x->p,y_n->r_n] S           exp_app
==> G              H              p r_n            _                S

    G              H              fix f-1 y_n      E[y_n->r_n]      S           fix
==> G              H[p->Hole]     f-0 r_n+p        _                p^U : S     (fresh p)

    G              H              C y_n            E[y_n->r_n]      S           exp_con
==> G              H              C r_n            _                S

    G              H              x pi n           E[x->p]          S           exp_sel
==> G              H              p pi n           _                S

    G              H              try X catch h    E[h->q]          S           try
==> G              H              X                E[h->q]          q^C : S    

    G              H              throw x          E[x->p]          S           throw
==> G              H              Except p         _                S

    G              H[p->M]       (r->)p            _                S           var
==> G              H[p->M]        p->M             _                S

    G[f->\x_n->L]  H              p->f^u-0 r_n     _                S           update_1a
==> G              H[p->Hole]     L                [x_n->r_n]       p^U : S

    G              H              p->q r_n         _                S           update_1b
==> G              H[p->Hole]     q r_n            _                p^U : S

    G[f->\x_n->L]  H              (p->)f^n-0 r_n   _                S           update_1c
==> G              H              L                [x_n->r_n]       S

    G              H              p->f-k r_n       _                q^U : S     update_2a
==> G              H[q->p]        p->f-k r_n       _                S

    G              H              f-k r_n          _                q^U : S     update_2b
==> G              H[q->f-k r_n]  q->f-k r_n       _                S

    G              H              p->C r_n         _                q^U : S     update_2c
==> G              H[q->p]        p->C r_n                          S

    G              H              C r_n            _                q^U : S     update_2d
==> G              H[q->C r_n]    q->C r_n         _                S

    G[f->\x_n->L]  H              f-0 r_n          _                S           call
==> G              H              L                [x_n->r_n]       S

    G              H              fix f-1 y_n      E[y_n->r_n]      S           fix
==> G              H[p->Hole]     f-0 r_n+p        _                p^U : S     (fresh p)

    G              H[p->M]        p r_n            _                S           app_1
==> G              H[p->M]        p->M             _                r_n^A : S

    G[f->\x_m->L]  H             (p->)f-n q_l      _                r_n^A : S   app_2a
==> G[f->\x_m->L]  H              L                [x_m->q_l+r_n]   S

    G              H             (p->)f-m q_l      _                r_n^A : S   app_2b
==> G              H              f-k q_l+r_n      _                S    (m > n, k = m-n)

    G              H              case x of as     E[x->p]           S           case_1
==> G              H              p                _                 (as^M,E') : S   (E' = E|live(as))

    G              H             (p->)C q_m        _                 (as^M,E') : S   case_2a
==> G              H              A                E'[y_n->q_m])     S   (C y_n->A in as)

    G              H              p->C q_m         _                 (as^M,E') : S   case_2b
==> G              H              A                E'[z->p,y_n->q_m] S   (z@(C y_n)->A in as)

    G              H              C q_m            _                 (as^M,E') : S   case_2c
==> G              H[p->C q_m]    A                E'[z->p,y_n->q_m] S    (z@(C y_n)->A in as, fresh p)

    G              H              p->C q_m         _                 (as^M,E') : S   case_2d
==> G              H              A                E'[z->p]          S             (z->A in as)

    G              H              C q_m            _                 (as^M,E') : S   case_2e
==> G              H[p->C q_m]    A                E'[z->p]          S             (z->A in as, fresh p)

    G              H              p->f-k q_m         _               (as^M,E') : S   case_3a
==> G              H              A                E'[z->p]          S             (z->A in as)

    G              H              f-k q_m          _                 (as^M,E') : S   case_3b
==> G              H[p->f-k q_m]  A                E'[z->p]          S             (z->A in as, fresh p)

    G              H[p->M]        p pi n           _                S           sel_1
==> G              H[p->M]        p->M             _                n^P : S

    G              H             (p->)C q_m        _                n^P : S     sel_2
==> G              H              q!n              _                S

    G              H[q->M]        Except p        _                 q^C : S     catch_1
==> G              H              q->M            _                 p^A : S

    G              H              (q->)M          _                 q^C : S     catch_2
==> G              H              (q->)M          _                 S

    G              H              Except p        _                 X : S       unwind
==> G              H              Except p        _                 S
```

<!-- %% TODO add prim ops and IO -->

##### Source language
```
P ::= D+
D ::= f x* = do T
T ::= return E | M ; T | let x = E ; T | case E of {A+} | try E catch y; | if x then T else T
E ::= x | C x* | f x* | E x* | E pi n
```

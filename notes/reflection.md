# Reflections on the implementation of functional languages

### A memory driven evaluation strategy

The main difference between various evaluation strategies is in making the decision of when to evaluate something.
Strict evaluation has predictable performance characteristics by executing everything immediately, and lower overhead (due to not having to allocate a suspended computation and checking whether something is evaluated).
Lazy evaluation can save work by skipping things that end up to be never used, as well as stimulating reuse of functions where composition using partial results does not waste work.
Delaying evaluation (even if the corresponding computation will be always demanded) yields extra oppertunities for parallel execution.
The sharing difference between call-by-name and call-by-need evaluation is complicated tradeoff between recomputation and memory costs.
However, even the timing of evaluation has impact on the performance of a program, as the memory use of the arguments of suspended function is often different from the memory use of its evaluated result.
The memory consumption of a program is not only a hardware requirement, but also indirectly influences the execution time of a program.
More memory in use means more work for the garbage collector, and more pressure on capacity of the caches that will increase the average latency of a memory access.

In general to minimize memory foot print, you would like every suspended function that has larger arguments than results to be evaluated as soon as an idle core is available.
However, the size of the arguments may not be cheap to determine, thus the garbage collector might need to do some bookkeeping when it is traversing those arguments.
And how do you take in account sharing of argument values with other use sites, and arguments containing expensive suspended functions?
Both cases can eliminate all benefits of trying to be eager in evaluating memory reducers.
Next we need to determine which functions have the potential to reduce memory foot print, and are relatively cheap to executed compared to amount of memory saved.
This is a hard question in general, but a simple analysis in the compiler might give a good enough approximation (types and the overal structure of function tell a lot).
A final problem is that making evaluation more eager can waste work when the result of some function is never used.
Profiling information could give us a good guess about which function are likely to be left unevaluated.
But even evaluating functions which results are rarely demanded can be beneficial, if it saves the garbage collector a lot of copying work.
Thus memory driven evaluation will be integrated with garbage collection, or at least decided by it.
Making all this work is a large engineering problem and the gains to be had might not be impressive, but it is worthwile in a world where the number of memory accesses is primary performance bottleneck.   
<!--- relation with eager and stingy evaluation??? --->

### Optimizing for parallelism using more and smaller energy efficient cores

### Potential improvements for main stream processor architectures

* multiple word load and store operations on the non simd side of the processor
* better support for pointer tagging, maybe even some programmable logic for tag mask manipulation
* feed pointer tag information into branch predictor on indirect jumps

### Functional languages with support for explicit resource management

### Hardware support for runtime program specialisation

* drop the assumption the program is static information
* benefit from runtime information like just in time compilers can
* Thyer's lazy specialisation with hardware support?

### Full stack formal verification from compiler down to hardware gates

### Extensions for security

* stack is already processor controlled instead of by the program
* heap references are opaque for the program and we have no pointer aritmetic
* advanced type systems of languages such as Haskell and Idris help a lot in program correctness
* above three points can mitigate many security bugs
* overflow and bounds checking could be added at a low cost
* instruction set is high level enough that source program types could be preserved and checked when loading the program


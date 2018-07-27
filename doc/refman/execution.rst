Execution Model
===============

Felix has a novel and sophisticated execution model.
There are two novel features.

Inderminate Evaluation Strategy
+++++++++++++++++++++++++++++++

Whilst most languages specify eager evaluation, meaning
function arguments are evaluated before calling a function,
and a few, such as Haskell, specify lazy evaluation, meaning
the function is called immediately and the arguments
are evaluated inside the function on demand,
Felix uses indeterminate evaluation by default: that is;
it allows the compiler to choose the evaluation strategy.

Indeterminate evaluation is one reason why Felix is so fast.

When a function is inlined, the parameter can be replaced
in each case by the argument expression. This can be 
faster because further optimisations are possible.

On the other hand if the argument is used many times,
it may be better to evaluate it just once. Also, when a closure
of the function is formed, it is hard to substitute an as yet
unknown argument into the closure, so Felix uses eager evaluation
for closure arguments.

Because it is able to chose the fastest strategy, Felix generates
extremely fast code. In practice the semantics are the same
which every strategy is used for most functions, even if the
function is not total/strict, because it is used with arguments
for which the behaviour is the same anyhow. For example
division is not total because the result is not defined
if you divide by zero, but the result is the same for all
other values.

Felix provides several ways to modify the default behavious.
If a function parameter is marked `var` then even if the function
is inlined, it will evaluate the argument and assign it to
the parameter *provided the parameter is actually used*; that is;
the function will behave as if eagerly evaluated.

On the other hand if you want lazy behaviour you can make
your function accept a closure end evaluate it when you
need the result.

Fibres
++++++

Felix supports shared memory concurrency with conventional
pre-emptive threads. However within each thread a collection
of fibres may run.

Fibres are logical threads of control which communicate with
other fibres using synchronous channels, as well as shared
memory. Fibres interleave execution in a pthread under program
control: I/O operations on channels cause the current fibre
to be suspended until at least a matching opposite operation
of another fibre. Reads match writes, so a reader will suspend
until a write provides the data it is waiting for, writers
suspend until there is a reader to consume the data it provides.

In a fibrated system all events form a total order: fibration
is a *sequential programming technique* in which pieces of the
program suspend of their own volition, and are resumed by 
a scheduler at the earliest when their I/O requirement is met.

Fibres communicating with anonymous channels cannot deadlock,
deadlock is a legitimate method of suicide. When a fibres
starves for input, or blocks for output, which cannot arrive,
the fibre is removed (by the garbage collector) and so a deadlock
is equivalent to the fibre exiting.

The details are beyond the scope of this brief description
and can be found elsewhere.


Asynchronous Event Handling
---------------------------

Felix has an asynchronous event handling system which currently
supports two kinds of events: timeout of an alarm clock,
and socket readiness notifications.

When utilised a separate system pthread monitors timers and
sockets using the best available technology for the current
platform: it chooses between select, poll, epoll, kqueue,
Windows completion ports and Solaris completion ports.

When a coroutine waits for a clock alarm, or requests a socket
read, write, or connection, its fibre is blocked until the
alarm triggers, or the requestion socket I/O operation is
complete. However *other* coroutines running on the same
pthread do not block.

Garbage Collection and Threads
++++++++++++++++++++++++++++++

Felix runs with a garbage collector. Use of the collector is
"optional" in a certain sense. The collector is a naive
world-stop mark/sweep variety, with a parallel mark algorithm.

The collector can be called manually, or is triggered by
an attempt to allocate memory when a threshold is reached.
Felix uses an allocator which itself calls the C library
`malloc`. The collector is precise with object allocated
on the Felix heap, tolerates but cannot scan objects allocated
on the C heap, and conservative on the machine stack.

When a collection is triggered it must wait until all 
threads stop. Threads will stop only when they attempt
an allocation or explicitly check for the GC pending flag.
Because of this Felix provides its own versions of synchronisation
primitives such as mutex and semaphores. Raw system primitives
can only be used with knowledge of the memory and execution 
model. For example a raw mutex is fine provided the scope of
the lock does no allocations and completes in bounded time.
The Felix mutex is actually a spin lock that checks the 
GC flag whilst spinning (and also inserts an small
OS based sleep in the spin loop to encourage the holder of
the lock to run and complete.


Closures
++++++++

It is important to note variables *including* `val`s are
part of a stack frame object which may be allocated
on the heap and the capture is via a pointer to the
whole frame. This means when a closure is executed,
the value of the captured variable is the value
current *at the time the closure executes* and not
the value at the time of capture.

For example:

.. code-block:: felix

  var x = 1;
  var g: (1->0)^3;
  noinline proc f (y:int) () { println$ y; };
  for i in 1..3 do
    &g.(i - 1) <- f x; // capture value x in y
    ++x;
  done
  for i in 1..3 do
    g.(i - 1) (); // prints 1,2,3
  done

Without the `noinline` Felix is too smart and the variable
y is inlined to the mainline, so there is only one copy,
when you run the script, it prints 4,4,4. If you just change
the parameter to `var y:int` to force eager evaluation,
it prints 3,3,3.

Capture by address is not a design fault, it is in fact
the only option. Just consider:

.. code-block:: felix

  var x = 1;
  fun getx() => x;
  ++x;
  println$ getx();

You would be surprised if this printed 1! You expect
the function to report the current value of x.



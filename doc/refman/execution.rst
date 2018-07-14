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

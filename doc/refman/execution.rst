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

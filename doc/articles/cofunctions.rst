===========
Cofunctions
===========

Yield
=====

In many programming languages today, there is a special
instruction usually called `yield` which allows a procedure
to return a value, suspending its execution state in such
a way it can resume where it left off.

.. code-block:: felix

    gen producer () : int = {
     var i = 0;
    again:>
     yield i;
     ++i;
     goto again;
    }

This is a simple example of what is called a `yielding generator`
in Felix. A `generator` is a function like construction which
may return a different value on each invocation, depending
on some mutable state. The prototypical generator is `rand`
which returns a random number.

The `yield` instruction seen above is similar to `return`,
however the producer above does not loose its current
location or local variables when it provides a value,
instead it suspends so that it may be resumed and
continue on where it left off.

You can use the generator above like this:

.. code-block:: felix

    var next = producer;
    var current = next();
    while current < 10 do
      println$ current;
      current = next();
    done

The key here is that the variable `next` is used to store
the suspended state of the producer as a closure, which
is resumed by each call.

Iterators
=========

This particular kind of construction is also known as
an `iterator` in Python. In C++ it is called an
`input iterator` although the use is slightly different,
and the definition is via a class object.

In general, iterators can be constructed in any object
oriented language using an object with mutable state and
a `get` method which simultaneously returns a fresh value
and also updates the state so the next value can be calculated.

However, OO based iterators are weak compared to yielding
generators because the `yield` instruction automatically saves
the current location in the generator.

Cofunctions
===========

I want you to see that an iterator is roughly a function
`turned inside out` and therefore deserves the technical
name `cofunction`. The natural output of an iterator is
a `stream`, but there is more: it a temporal stream.

Functional programming models have a very serious weakness
which is that they attempt to be `atemporal`. Advocates
laud the fact that an FPL is primarily declarative.
Data structure are indeed spatial, but there is more
to programming that space, and more to programming than
data and functions.

Cofunctions provide a space time transform. You can take
a list and produce a stream. A purely spatial, linear
data structure has been converted actively into a temporaly
linear sequence of codata.

Where data lives at addresses, some of which may be adjacent,
and others linked indirectly by pointers, codata has
temporal coordinates, marked by a clock.

The world of space and time provide the coordinate system
of a program, and the flow of control explains how an
algorithm looks at one location at one time, but progresses
the construction of new data by simultaneous spatial and
temporal sequencing. You move down the list, physically,
and you do so in time.

A cofunction, therefore, is an iterator over an
abstract data structure, which produces a stream 
of values. The stream has no natural end and this
is a fundamental property which is entirely misunderstood.

Many people think streams are infinite lists but this
is in fact completely and utterly wrong. It is hard to
comprehend how such fundamentals are so badly misunderstood.

Contrary to popular belief, inductive data types like lists
are infinite, whereas streams are finite! It is not hard
to understand when one realises that computing is like
science not mathematics, it requires a concept of
`observation`.

Suppose I give you a list and ask you how long it is:

.. code-block:: felix

   fun len (x: list[int]) => match x with
    | Empty => 0
    | Cons (head, tail) => 1 + len x
  ;

You would use that algorithm and say that

.. code-block:: text

    [B] -> [C] -> [D] -> *

was length 3. But, you would be wrong! You see, I didn't
show you the whole list:

.. code-block:: text

    [A] -> [B] -> [C] -> [D] -> *

I only showed you the tail starting at B. Now you realise
your answer should have been `at least 3`. That is the 
correct answer because the algorithm for the length counts
in time by following pointers to the end, but it is a singly
linked list so you cannot go backwards!

Let me say that again another way: irrespective of what
exists in space, or not, the only thing that matters is
what you can observe by an `effective procedure` which
is also called an `algorithm`.

So we can observe only a `lower bound` of the size of
a list because we only ever see the `tail` of the list.
You can never tell, or, `measure` if the first element 
you see is the head of the list.

So we must emphasise again the relativistic nature of
computing: it is all about what you can observe,
not about what is. So inductive data types, like lists,
all have the same structural property that observations
are finite, but are necessarily only lower bounds.

Because the list `could be` longer than any calculated
bound we have to assume it is, because no observation
can contradict that assumption, in other words,
lists are infinite!

Now it is vital to understand that a functional
observation of some property, is intrinsically bounded.
Suppose you write a function and make a mistake and
write an infinite loop. The function never returns,
so it is not, in fact able to be used to make
an observation: it is not an algorithm.

So I am now going to blow your minds, by claiming
that due to duality, streams are finite, and,
in fact, when you make an observation on a stream,
you are producing an `upper bound`.

Suppose you have a an iterator producing a stream
of all the integers. You might think, this is 
an infinite stream but you could not be more wrong!
If it were infinite, an program using the stream
to perform a calculation would never terminate!

Let us see how to measure the length of a stream:

.. code-block:: felix

    gen observer () : int = {
      var next = producer;
    again:>
      var current = next();
      println$ current; // observation
      yield current;
      goto again;
    }

Now here is the critical thing: to actually
use a stream and calculate some value, we have
to `impose` a bound on our observations:

.. code-block:: felix

    gen sum () : int = {
      var it = observer;
      for i in 0 ..< 10 perform
         x += it();
      return x;
    }
 
Now if we call sum, how many prints do you see?
Did you say 10? So you think, the stream is at least
10 long but, you have it arse about.

Suppose you only saw two:

.. code-block:: felix

    gen producer () : int = {
     var i = 0;
     yield i;
     yield i + 1;
     yield i + 2;
     again: goto again;
    }

If you see this producer it produces 3 values, then it
goes into an infinite loop. So you can write code
that reads the first 4 values from it, and that code
will never return. It does not make any observation.
If you reduce the number to 3,2,1, or 0, you get an observation.

Now think about the producer code itself, and ask, how many
values does it produce? Well, if it is called 10 times
it produces 3 values. If it is called 9 times it produces 3 values.
if it is called twice it produces 2 values. And it if it is never
called, it produces NO values.

So the number of values produced by our iterator above is what?
You got it! It is `at most 3`.

Streams, by their nature, are finite, not infinite!
They are characterised by an upper bound.

All streams are finite, in the sense of a program being
a terminating algorithm, and functions, necessarily,
most complete and return a value or they're not functions.


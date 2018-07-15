======================
Using Uniqueness Types
======================

Ownership
=========

Uniqueness types provide a way to help enforce a 
contract of exclusive ownership. Lets look at an example
to see how they work.

Raw Operations
--------------

The first thing we're going to do is provide a Felix binding to
some basic C string manipulation routines.

Raw Memory Manipulation
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: felix

    class Memory
    {
      gen malloc: !ints -> address = 'checked_malloc($1)' 
        requires Cxx_headers::cstdlib, checked_malloc
      ;

      proc free: address = 
        "::std::free($1);"
        requires Cxx_headers::cstdlib
      ;

      gen realloc: address * !ints -> address = 
        "::std::realloc($1,$2)"
        requires Cxx_headers::cstdlib
      ;
    }

Raw C String Manipulation
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: felix

    class CString
    {
      //$ C strcpy.
      proc strcpy: +char * +char = 
        "(void)::std::strcpy($1,$2);" 
        requires Cxx_headers::cstring
      ;

      //$ C strncpy.
      proc strncpy: +char * +char * !ints = 
        "(void)::std::strncpy($1,$2,$3);" 
        requires Cxx_headers::cstring
      ;

      //$ C strlen: NTBS length.
      fun strlen: +char -> size = "::std::strlen($1)" 
        requires Cxx_headers::cstring
      ;

      fun len (s:+char) => strlen s;

      //$ Traditional NTBS strdup.
      gen strdup: +char -> +char = 
        "::flx::rtl::strutil::flx_strdup($1)"
        requires package "flx_strutil"
      ; 
    }

Problems with Raw Operations
----------------------------

The raw operations shown above are difficult to use safely,
whether you're writing code in C, C++, or Felix. 

What can go wrong?


Forgetting to Free
~~~~~~~~~~~~~~~~~~

After you `malloc` a string, you have to eventually
`free` it or you get a memory leak. It is easy to forget
to do this.

Double Free
~~~~~~~~~~~

An even worse problem is freeing the memory you allocated 
twice. This may cause your program to abort, particularly
if you have a modern checking allocator. Historically,
however, the double free simply corrupted memory, and 
all sorts of weird things could happen. If you were lucky,
you would soon get a core dump, if you were unlucky, your program
ran but produced the wrong results.

Dangling Pointers
~~~~~~~~~~~~~~~~~

Another problem which occurs is that you free the
pointer, but then go on and use it anyhow. This is known
as a dangling pointer, since it points off into the
wild blue yonder. Again, if you're lucky, you might get
a program abort quickly, but it is easy to be unlucky.

Invalid Pointers
~~~~~~~~~~~~~~~~

Because allocators often maintain free lists, with the most
recently freed memory at the top, if you free your memory,
thinking it is gone, but retain a pointer to it somewhere,
then allocate a new block of memory, it is quite likely to be
to the same location as the last memory you freed.

Now, your old pointer is not actually dangling, it is pointing,
unexpectedly, into the new string you allocated. If you then
write through that pointer, the memory is changed at mysterious
modifications appear in the string, even though you have not
made any changes through the new pointer you allocated.

C++ Strings: Encapulsation
--------------------------

The very first class every C++ programmer wrote
was a string class.

The technique used was basic object orientation.
The idea is to hide the pointer in a C++ class,
by making it a private member,
and provide public methods that safely manipulate the string,
without revealing it to the programmer.

This technique is using *abstraction* for the purpose
of *hiding representation details*. It is a good method,
but C++ string classes had their problems.

Lack of facilities
~~~~~~~~~~~~~~~~~~

A key problem with any string class is that since you cannot
access the underlying pointer directly, you may want to do
something to the string which either cannot be done,
or can only be done inefficiently.

Most string classes programmers wrote started off simple,
but the programmer had to come back time and again,
to add new methods to the class so things could be 
both efficient and safe.

For this reason most string classes acquired a mix of
two flavours to solve this problem: the first was to provide
a rich, kitchen sink of methods that covered as much as
experience showed was required.

The second method was to provide a cheat method that did
actually expose the underlying pointer. 

Copying
~~~~~~~

A second serious problem with string classes was that in
order to ensure the user could modify the string,
without the modifications turning up unexpectedly
in someone elses string, the string had to be copied
quite often. 

C++ did this copying using a copy constructor, so that,
for example, when you pass a string to a function,
the function is free to modify it.

The cost of copying is reduced in C++ by using const
references, however this method is not safe either.
The problem is, the same string can be passed as both
a const and non-const reference, and the function
receiving them can modify the non-const version,
and the const version mysteriously changes.

This is an example of a general class of problems
known as aliasing problems, characterised by the
existence of a single object with multiple names,
or, more precisely, multiple access paths.

Move Constructors
~~~~~~~~~~~~~~~~~

In C++11 a major advance was made due to the introduction
of rvalue references. An rvalue reference can only bind
to an rvalue, and rvalues are always unique. So an rvalue
passed to a function with an rvalue reference parameter can safely
modify the underlying memory, because the type system
ensures it is the exclusive owner.

C++ uses this feature primarily by allowing a so-called
move constructor, which, instead of copying the underlying
memory, simply moves the pointer from the argument object
to the parameter object, leaving the argument evacuted.

It helps a lot, providing reasonable safety and improved
performance, but we can do better!

What is the Real Problem?
-------------------------

We need to fully understand the actual problem here.
The difficulty arises because the pointer and the
memory it refers to are *decoupled*. They're different
things that have to be kept in sync. You can copy the
memory, and copy the pointer, separately.

The C++ class enforces coupling, and it enforces ownership
by copying. The more recent use of move constructors
leverages the type system to gain performance by 
using the knowledge that rvalues are unique.

So the problem is about coupling, and we can state this
another way: its about ownership. If there are many
copies of the pointer for one memory block, ownership
is shared. If there is only one, it is exclusive.

If we can enforce exclusive ownership, mutations are
always safe, in particular, exclusive ownership
implies a one to one correspondence between the
representative of a value and the value itself.
It means, if the string is deleted, the pointer
must be inacessable.

Uniqueness Types
================


Felix provides some machinery to further aid in
establishing and maintaining knowledge of, and the
ability to, reason about ownership: uniqueness types.

The facility is used to enforce a contract, but it does
not provide a global safety guarrantee. Our system
provides a type constructor `uniq` which can be 
applied to any type.

We also provide three operators which can be applied to
expressions. The `box` operator takes a value of some
type T, and returns a value of type `uniq T`. The 
`unbox` operator takes a value of type `uniq T`,
for some type T, and returns a value of type T.

These operators are type coercions which have no
run time impact. The uniq typing is erased by
the compiler in the back end after type checking,
ensuring there is no run time penalty for unique
typing.

In addition there is an unsafe cheat operator
`peek` which can be applied to a read-only pointer
to store of a uniq type, which returns the 
stored value.

Finally there is an unsafe procedure `kill`
which consumes a uniq value without doing 
anything. It can be used when the value
has been consumed in a way that has escaped
the notice of the type system, such as by
use of a pointer, not notify the type system
that the value is dead. It has no utility
unless applied to a variable.

.. code-block:: felix

    open class Unique 
    {
      // box up a value as a unique thing
      fun box[T] : T -> _uniq T = "($t)";

      // unsafely unpack the unique box
      fun unbox[T] : _uniq T -> T = "($t)";

      // kill a live unique value
      proc kill[T] : uniq T = ";";

      // functor for typing
      typedef fun uniq (T:TYPE):TYPE => _uniq T;

      // peek inside the box without changing livenes state
      fun peek[T] : &<(uniq T) -> T = "*($t)";
    }
     
In this code the type operator `_uniq` is the compiler
intrinsic, the functor `uniq` is used to provide the
public version.

The system also provides conversions
to strings which delegate to the conversions for
the underlying type, not shown here.


Example: UniqueCStrings
-----------------------

This example presents a cut down version of the
Felix standard library component `UniqueCStrings`
which illustrates a real use of uniqueness typing.

Setup
~~~~~

We are going to use a class to encapsulate our
methods so we start like this:

.. code-block:: felix

    open class UniqueCStrings
    {
      open CString;
      open Memory;

We have included the unsafe raw operations inside
the class privately for our use. 

Abstraction
~~~~~~~~~~~

We're going to steal the OO idea and make out
representation type abstract.

.. code-block:: felix

      // abstract representation
      private type _ucstr = new +char;

In Felix, the `type` binder introduces an abstract type.
The RHS of the construction may be either the `new` operator
followed by a type expression, or it may be a C++ type
wrapped in a string. In the above the type `+char` means
an incrementable non-null pointer to an array of `char`.

As well as being abstract, we're also preventing the 
name `_ucstr` from being visible outside the enclosing
class `UniqueCStrings`.

Uniqueness
~~~~~~~~~~

We're going to make the publically visible
version a unique type.

.. code-block:: felix

    // make it uniq
    typedef ucstr = box _ucstr;

The type constructor `uniq` specifies a uniquely
typed version of the type it qualifies.

Internal Access
~~~~~~~~~~~~~~~

Felix enforces abstraction fully. It does this by
providing two methods for an abstract type,
both of which are private so they can only be used
inside the class defining the abstract type.

These methods are `_repr_` which casts the abstract
type to its implementation, and `_make_ucstr` which
casts the representation to the abstraction.

Since these are a bit messy to write, we will provide
private wrappers functions:

.. code-block:: felix


    // privatise access to representation
    private fun pack (p: +char) => p._make__ucstr.uniq;
    private fun unpack (var p: ucstr) : +char => p.unbox._repr_;

You should think of `pack` as a way to take
a raw char pointer and wrap it up in a package
you can move about. You can pass this box from
one variable to another. If a function has a local
variable, we can say the function owns that variable.
The variable is like a cupboard, into which you can
put things.

You can take the box out of one cupboard,
and put it it another, but you cannot easily
copy the box, because you cannot see inside it.

When you have a new box put in your cupboard,
you can take the box out of the cupboard
and `unpack` it to find out what is inside.
You can then play with its contents with
relative safety, knowing that it is exclusively yours
to play with.  Its important to note, there isn't
space to unpack your box inside the cpuboard. Its like
a mail box, you have to take your letter out, emptying
the mail box, before you can open the letter.


Constructors
~~~~~~~~~~~~

Now we need a constructor. We're going to use a C++ string
which is a Felix string and copy its value into an a C array
using the method `_unsafe_cstr`:

.. code-block:: felix

    // Constructors
    ctor ucstr (var s:string) = {
       // malloc'd copy of string contents
       var p =  s._unsafe_cstr; 
       return pack p;
    }

What `_unsafe_cstr` does is malloc a new array and copy the contents of the
C++ internal array into it, it used the C++ method `c_str()`
to gain access to the internal C array. This is the C++ `string` class
cheat method. Our `_unsafe_cstr` is not safe because it is returning a raw
pointer which we might forget to free, but we're trying to fix
that by coercing it to a unique type. By wrapping into a box.

Another constructor just copies an existing C string
and packs it up into a unique type:

.. code-block:: felix

    ctor ucstr (s:+char) => s.strdup.pack;

You can see here our code is doing unsafe things, with raw
C strings, but we are then using `pack` to at least
notify our client. Because the constructor returns a
unique type, the client of the constructor believes they
have exclusive ownership of the returned value.

And that indeed is the intent and purpose of our 
constructor code, and we can easily verify we have
met our part of the bargain.

Destructor
~~~~~~~~~~

We need to provide a way to free our string:

.. code-block:: felix

    // deletes the store
    proc delete (var p:ucstr) {
      var q = unpack p;
      free q;
    } 

How do we know it is safe to free the underlying pointer here?
The answer is, as the client of the unique value, we are
entitled to believe that we are the exclusive owner of it.
It has been moved to the variable `p` which is exclusively
our variable, and it has a type which indicates it holds
an exclusively owned value.

The type system cannot enforce the exclusive ownership,
but it does enforce a useful contract. It ensures that
if the caller *claims* to be the exclusive owner by
typing the argument value `uniq` then that claim
will be recognised by the callee routine because
the type system will *ensure* that the parameter is
also typed `uniq`.

In other words, this is a coupling contract. The argument
passing to the parameter is a contract of transfer of
ownership *witnessed* by the type system. All bets are
off, before the client signs the contract by wrapping
the value with a `uniq` operator. All bets are off,
after the service routine signs the contract by 
accepting the packed value, and then unpacks it.

What the contract enforces is an agreement that
the value is `moved` from the client routine
to the service routine, instead of being copied.


Display
~~~~~~~

We have to be able to see our strings. We already have facilities
to see Felix strings which are bindings of C++ strings. So we
will leverage our generic classes `Str` and `Repr` which allow
us to specify a default way to convert any particular type 
to a standard string:

.. code-block:: felix

    instance Str[_ucstr] { fun str(p:_ucstr)=>p._repr_.str; }
    instance Repr[_ucstr] { fun repr(p:_ucstr)=>p._repr_.repr; }

    inherit Str[_ucstr];
    inherit Repr[_ucstr];

The instances there implement the functions `str` and `repr`
which are for displaying as human readable text and 
machine readble programming language level literals,
respectively.

Then, we yank bindings to these methods into our class
with the `inherit` directives.

What is important to observe about these routines is that
they do not operate on the unique type. The values that they
receive are abstracted representations of the underlying
C pointer which we observe using the private `_repr_`
method. What this means, is that you cannot apply the
`str` operator to a packed up box, it won't work on a `uniq`
value. 

The box has to be unpacked first.  Here, we're using abstraction
to ensure that we can provide this operation to the user
without needing to expose the underlying pointer, but the user
must already own the value and have taken responsibility for its
safe management by unpacking a unique value.

Length
~~~~~~

How long is a piece of string?

.. code-block:: felix

    // length
    fun len(var s:&<ucstr) : size => s.peek._repr_.strlen;


This method accepts a read-only pointer to a `ucstr`
because it only wants to inspect it. Such a pointer
is unsafe in general! We need to examine the code carefully
to see that whilst we have unsafely acquired a reference
to the `ucstr`, we have used it and then promptly forgotten it.

The pointer we acquired is an `alias` so retaining it would
threaten ownership. But we used it and forgot it immediately,
so all is well. The `peek` operator you see above is used
to look inside a unique type, which is not safe in general,
it is only safe if you only take a peek.

The machinery of taking the address of a `uniq` value and 
then passing it to a client is known as `lending` the value.
It is not safe in general to lend something to a service 
you do not trust. We can trust this function, because
we can see its implementation.  In particular we can
see that whilst it does `peek` inside the packed up box,
in order to calculate the length of the string, it does
not pass on the secret knowledge to anyone else, it
returns only the length, not the pointer to the C array
it peeked at. Similarly, the `len` function itself
has trusted that the C `strlen` function has only used
the supplied pointer transiently.

Modifying One Character
~~~~~~~~~~~~~~~~~~~~~~~~

Now we're going to modify one character. We use
an unsafe function, `Carray::set` to so this.
It operates on a pointer to a C array, or `+char`
type. Here we make no assurance that the location being
set is inside the string. This operation, therefore
is unsafe, in that the index could be out of bounds.
Our concern here is not with the validity of the bound,
but that, assuming the bound is indeed valid,
we can safely modify the string in place and return
it, without anyone observing we did so.

 
.. code-block:: felix

    // modify one char
    fun set (var s:ucstr, i:int, c:char) : ucstr =  {
      var cs = unpack s;
      Carray::set (cs, i, c); 
      return cs.pack;
    }

Now, we reason that, because `s` has a `uniq` type, we are the 
sole owner, so no one else can observe any change we make.
So instead of making a copy of the string and modifying
it, we can safely modify the original.

That is the key to uniqueness types.
Using the type system to reason that a mutation cannot
be a side effect, because, whilst there is actually
an effect in the form of a mutation, it can only be
observed in the output of the function. It cannot be
observed `on the side` because no one else knows about it.

To say this another way, the caller cannot know if the
string was modified, or a copy created and modified.
What we gain is this: if the caller believes they have
exclusive ownership, the caller can sign the transfer
of ownership contract by making the type unique.
In doing so they enable a significant optimisation
whereby the need to copy the string to avoid a
side effect is removed, a significant saving in
both time and memory.

The saving is actually considerably greater because
the system is relieved of the cost of calculating
whether the string is reachable from any live part
of the program, a calculation which is normally done
by the garbage collector.


Appending two strings
~~~~~~~~~~~~~~~~~~~~~

Now we get to a more difficult routine.

.. code-block:: felix

    // append: consumes y
    fun append (var x:ucstr, var y:ucstr): ucstr = {
      var cx = unpack x;
      var cy = unpack y;
      var lx = cx.len;
      var ly = cy.len;
      var r = realloc (cx, lx+ly+1);
      strncpy (r+lx,cy,ly+1);
      free cy;
      return pack r;
    } 

This routine takes two unique arguments, and returns their
concatenation. It destroys `both` the arguments in the processes.
We unpack the two arguments to their underlying pointers,
and use C to calculate their lengths. Then we reallocate
the first argument, which can either add more store to its
end, or make a completely new copy with extra store at the
end, and free the old string. 

Then we add the characters of the second argument at the
end of the first, in the space we just allocated, and
free the second argument.

You may ask, how do we know we can destroy these arguments
safely?

The answer is that we own them exclusively. No one else knows
about them, we can do what we like with them. Because they
have a `uniq` type.

In fact that is not quite correct. There is one thing we
cannot do with a uniquely typed value: forget it.
We're responsible for it, we cannot forget it.
We have to either return it, handing back ownership
to our caller, or free it. We freed the second argument in this routine
and returned the first, with modifications, and possibly at a new
location. Realloc took care of what to do if we needed a new
store: it freed the old store and allocated the new store for us.
That's what realloc is specified to do.

Nondestructive Append
~~~~~~~~~~~~~~~~~~~~~

Here's how to avoid destroying the second argument:

.. code-block:: felix

    // append: doesnt consume y
    noinline fun append (var x:ucstr, var py:&ucstr): ucstr = {
      var cx = unpack x;
      var cy = py.peek._repr_;
      var lx = cx.len;
      var ly = cy.len;
      var r = realloc (cx, lx+ly+1);
      strncpy (r+lx,cy,ly+1);
      return pack r;
    } 

This is similar to our destructive version, however
because a pointer was passed as the second argument
instead of a value, we know we're only allowed to
peek at it. 

The type system will not stop you, if, instead of just
peeking, you dereference the pointer, unpack the
resulting value, and then free it.

Felix uniq types do *not* ensure correct usage.
What they actually do, is make the contract explicit.
There is enforcement, but it is not complete.

Convenience wrappers
~~~~~~~~~~~~~~~~~~~~

Here are some convenience wrappers:

.. code-block:: felix

  // nicer appends
  fun + (var x:ucstr, var y:ucstr) => append (x,y);
  fun + (var x:ucstr, var py:&ucstr) => append (x,py);

  proc += (var lhs: &ucstr, var rhs: ucstr) => 
    lhs <- append (*lhs,rhs)
  ;
  proc += (var lhs: &ucstr, var rhs: &ucstr) => 
    lhs <- append (*lhs,rhs)
  ;

These wrappers allow you to use the infix `+` functional
operator and `+=` procedural instruction. Note carefully
the procedural implementations! By dereferncing the
`lhs` pointer we have created a uniq value. We were
only given a loan, so is this safe?

In general, it isn't safe. But we can see here that,
although we have abused the loan by modifying the 
value, we are then storing the modified value back
into the original location. We have taken the box,
opened it, changed the contents, and put them back.
The original owner remains the owner, although
the value they own has changed.

Of course, that is not only what the owner expected,
it is what the owner demanded! There's no point putting
a broken phone in for repair, if the repair shop doesn't
actually fix it!

Enforcement: An example of usage
--------------------------------

Finally here is a simple example of usage.

.. code-block:: felix

    proc test() {
      var s = ucstr "hello";
      println$ &s;
      s = set (s, 0, char "e"); 
      var s2 = s;
      println$ &s2;
      delete s2;
    }
    test();

This snippets hides something I haven't explained yet.

See how I copied the value in variable `s` to `s2`?
*No you don't!*

I didn't copy it. I *moved it*. Felix enforces a special rule
for uniq types. They cannot be copied, only moved.

Programming languages have no natural syntax for movement,
only copying. So we need some help, when we do an assignment
and we really mean to move, and not copy the value.
Below I explain how we do that.

A variable of a uniq type has to be used *exactly once*.
If you pass the value in a variable to a function, the variable
goes out of scope and cannot be accessed. You owned the value,
but you gave away ownership.

If you do an assignment like the one from `s` to `s2` the
same thing happens. `s` loses the value and `s2` gains it.
Felix won't let you use `s` now.

But `s` was already used! Yes, it was passed to `set`, but
set then reinitialised it. Every variable of a unique type
is either initialised or dead. Assignments from one variable
to another kill the first variable and liven the second
one. The first one has to be alive to start, and the second
one has to be dead. After the assignment, the first one
is dead and the second one is alive. Life has been moved
from one variable to another. We actually say
that we have transfered `ownership`, as if you have
a dog in a kennel and then give the dog as a gift
to a friend (who puts it in another kennel, or lets it
sleep on the sofa). 

Taking the address of a variable does not kill it,
which means if you do take the address you must only
use it whilst the variable remains alive. Felix
does *not* enforce that.

So here you see the contract. Felix enforces correct
use of whole variables, the programmer must enforce
the correct use of pointers.

Errors
~~~~~~

So what happens if you make a mistake?
Let me show you:

.. code-block:: felix

    proc test () {
      var x = box 1;
      println$ unbox x;
      println$ unbox x;
    }
    test;

Here we broke the rules. We used x twice. And here is what
Felix has to say about it:

.. code-block:: text

    ~/felix>flx tmp
    Once error: Using uninitialised or already used once variable
    (50313:->x)
    Variable x defined at
    /Users/skaller/felix/tmp.flx: line 3, cols 3 to 18
    2: proc test () {
    3:   var x = box 1;
         ****************
    4:   println$ unbox x;

What Felix does is a control flow analysis. The requirement
is that on every *statically* possible control path,
a uniquely typed variable alternates between two states:
`live` and `dead`. A variable is dead until it is initialised
or assigned to. Parameters of a function are considered live,
since we assume they were initialised by the caller with
an argument. 

If the variable is passed to a function, it must be live,
but at the point it is passed it is now killed and considered
dead. Some people say the value has been `consumed`.

A dead variable can be relivened by assigning it a new
uniq value.

At the end of a function, all uniqely typed variables
must be dead.

Felix does *NOT* recognise taking the address of a variable
as significant, *except* in the special case the address is
immediately used as the first argument of the store at 
operator `<-`. Tracking pointer aliases is not impossible but
it is hard to do properly, and it can be very expensive.
Felix is a lazy cat: he helps you get things right but 
won't force you.

The idea here is simple: a live variable contains a wrapped
box, a dead variable does not. When you move a value out
of a variable, it is no longer in the cupboard so the variable
is marked empty or dead. When you put something back in, the
cupboard is full and the variable live again.



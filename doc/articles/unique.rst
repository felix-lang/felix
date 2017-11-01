======================
Using Uniqueness Types
======================

Ownership
=========

Uniqueness types provide a way to help enforce an 
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
if you have an modern checking allocator. Historically,
however, the double free simply corrupted memory, and 
all sorts of weird things could happen. If you were lucky,
you soon get a core dump, if you are unlucky, your program
runs but produces the wrong results.

Dangling Pointers
~~~~~~~~~~~~~~~~~

Another problem which occurs is that you free the
pointer, but then go an use it anyhow. This is known
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

The second method was to provide an cheat method that did
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
or rvalue references. An rvalue reference can only bind
to an rvalue, and rvalues are always unique. So an rvalue
passed to a function with an rvalue parameter can safely
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
----------------

Felix provides some machinery to further aid in
establishing and maintaining knowledge of, and the
ability to reason about ownership: uniqueness types.

The facility is used to enforce a contract, but it does
not provide a global safety guarrantee.

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

Uniqueness
~~~~~~~~~~

And we're going to make the publically visible
version a unique type.

.. code-block:: felix

    // make it uniq
    typedef ucstr = uniq _ucstr;

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
    private fun unpack (var p: ucstr) : +char => p.ununiq._repr_;
    private fun pack (p: +char) => p._make__ucstr.uniq;

Constructors
~~~~~~~~~~~~

Now we need a constructor. We're going to use a C++ string
which is a Felix string and copy its value into an a C array
using the method `_unsafe_cstr`:


.. code-block:: felix

    // Constructors
    ctor ucstr (var s:string) = {
       var p =  s._unsafe_cstr; // malloc'd copy of string contents
       if debug perform
         println$ "Creating " + p.repr + " @" + p.address.repr;
       return pack p;
    }

What this does is malloc a new array and copy the contents of the
C++ internal array. It is not safe because it is returning a raw
pointer which we might forget to free, but we're going to fix
that by coercing it to a unique type.

Another constructor just copies an existing C string
and packs it up into a unique type:

.. code-block:: felix

    ctor ucstr (s:+char) => s.strdup.pack;

Destructor
~~~~~~~~~~

We need to provide a way to free our string:

.. code-block:: felix

    // deletes the store
    proc delete (var p:ucstr) {
      var q = unpack p;
      free q;
    } 

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

Modifying One Character
~~~~~~~~~~~~~~~~~~~~~~~~

Now we're going to modify one character. We use
an unsafe function, `Carray::set` to so this.
It operates on a pointer to a C array, or `+char`
type. It will never write past the end of a the array,
but the onus is on the caller to ensure the array pointed
at actually exists, and hasn't, for example, been freed.
The owner also has to be sure the string has a null terminator!

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
We have to either return it, handling over ownership
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
peeking, you dereference the pointer, unpacked the
resulting value, and then freed it.

Felix uniq types do *not* ensure correct usage.
What they actually do, is make the contract explicit.
There is enforcement, but it is not complete.


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
from one variable to another.

Taking the address of a variable does not kill it,
which means if you do take the address you must only
use it whilst the variable remains alive. Felix
does *not* enforce that.

So here you see the contract. Felix enforces correct
use of whole variable, the programmer must enforce
the correct use of pointers.









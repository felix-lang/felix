Executable Statements
=====================

Assertions
----------

`Reference <http://felix-lang.org/share/lib/grammar/assertions.flxh>`_

assert
^^^^^^

Ad hoc assertion throws an assertion exception if its argument
is false. 

.. code-block:: felix
   
   assert x > 0;

axiom
^^^^^

An axiom is a relationship between functions, typically
polymorphic, which is required to hold.

.. code-block:: felix
   
   axiom squares (x:double) => x * x >= 0;
   class addition[T]
   {
     virtual fun add : T * T -> T;
     virtual fun == : T * T -> bool;
   
     axiom assoc (x:T, y:T, z:T) : 
       add (add (x,y),z) == add (x, add (y,z))
     ;
   }

In a class, an axiom is a specification constraining
implementations of virtual function in instances.

Axioms are restricted to first order logic, that is, they
may be polymorphic, but the universal quantification implied
is always at the head.

Existential quantification can be provided in a constructive
logic by actually constructing the requisite variable.

Second order logic, with quantifiers internal to the 
logic term, are not supported.

lemma
^^^^^

A lemma is similar to an axiom, except that is it
easily derivable from axioms; in particular,
a reasonable automatic theorem prover should
be able to derived it.

theorem
^^^^^^^

A theorem is similar to a lemma, except that it is 
too hard to expect an automatic theorem prover
to be able to derive it without hints or assistance.

There is currently no standard way to prove such hints.

reduce
^^^^^^

A reduce statement specifies a term reduction and is logically
equivalent to an axiom, lemma, or theorem, however it acts
as an instruction to the compiler to attempt to actually 
apply the axiom.

The compiler may apply the axiom, but it may miss opportunities
for application.

The set of reductions must be coherent and terminal, 
that is, after a finite number of reductions the final
term must be unique and irreducible. 

Application of reduction is extremely expensive and they
should be used lightly.

.. code-block:: felix
   
   reduce revrev[T] (x: list[T]) : rev (rev x) => x;



invariant
^^^^^^^^^

An invariant is an assertion which must hold on the state variables
of an object, at the point after construction of the state
is completed by the constructor function and just before the
record of method closures is returned, and, at the start and
end of every method invocation.

The invariant need not hold during execution of a method.

Felix inserts the a check on the invariant into the constructor function
and into the post conditions of every procedure or generator
method.

.. code-block:: felix
   
   object f(var x:int, var y:int) =
   {
      invariant y >= 0;
      method proc set_y (newy: int) => y = newy;
   }


Assignment
----------

`Syntax <http://felix-lang.org/share/lib/grammar/assignment.flxh>`_

Jumps
-----

The ``goto`` statement and label prefix
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Felix statements may be prefixed by a label
to which control may be transfered by a @{goto}
statement:

.. code-block:: felix
   
   alabel:>
     dosomething;
     goto alabel;

The label must be visible from the goto statement.

There are two kinds of gotos. A local goto is a jump
to a label in the same scope as the goto statement.

A non-local goto is a jump to any other visible label.

Non-local transfers of control may cross procedure
boundaries. They may not cross function or generator 
boundaries.

The procedure or function containing the label 
must be active at the time of the control transfer.

A non-local goto may be wrapped in a procedure closure
and passed to a procedure from which the goto target
is not visible.

.. code-block:: felix
   
   proc doit (err: 1 -> 0) { e; }
   
   proc outer () {
     proc handler () { goto error; }
     doit (handler);
     return;
   
     error:> println$ error;
   }

This is a valid way to handle errors.
the code is correct because ``outer`` is active
at the time that ``handler`` performs the
control transfer.

goto-indirect/label_address
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``label-address`` operator captures the address
of code at a nominated label. 

The address has type ``LABEL`` and can be stored in a variable.

Provided the activation record of the procedure containing
the label remains live, a subsequent ``goto-indirect`` can
be used to jump to that location.

.. code-block:: felix
   
   proc demo (selector:int) {
     var pos : LABEL = 
       if selector == 1 
       then label_address lab1
       else label_address lab2
       endif
     ;
     goto-indirect selector;
   lab1:>
     println$ "Lab1"; return;
   lab2:>
     println$ "Lab2"; return;
   }



Exchange of control
^^^^^^^^^^^^^^^^^^^

TBD

halt
^^^^

Stops the program with a diagnostic.

.. code-block:: felix
   
   halt "Program complete";

Calls
-----

call
----

The ``call`` statement is used to invoke a procedure.

.. code-block:: felix
   
   proc p(x:int) { println$ x; }
   call p 1;

The word ``call`` may be elided in a simple call:

.. code-block:: felix
   
p 1;

If the argument is of unit type; that is, it is the
empty tuple, then the tuple may also be elided in
a simple call:

.. code-block:: felix
   
   proc f() { println$ "Hi"; }
   call f (); // is equivalent to
   f(); // is equivalent to
   f;

procedure return
----------------

The procedural return is used to return control
from a procedure to its caller.

A return is not required at the end of a procedure
where control would otherwise appear to drop through,
a return is assumed:

.. code-block:: felix
   
   proc f() { println$ 1; }
   // equivalent to
   proc f() { println$ 1; return; }

return from
^^^^^^^^^^^

The return from statement allows control to be
returned from an enclosing procedure, provided that
procedure is active.

.. code-block:: felix
   
   proc outer () {
     proc inner () {
        println$ "Inner";
        return from outer;
     }
     inner;
     println$ "Never executed";
   }

jump 
^^^^

The procedural jump is an abbreviation for 
the more verbose sequence:

.. code-block:: felix
   
   jump procedure arg; // is equivalent to
   call procedure arg;
   return;

function return
---------------

The functional return statement returns a value from
a function.

.. code-block:: felix
   
   fun f () : int = {
     return 1;
   }

Control may not fall through the end of a function.

yield
^^^^^

The yield statement returns a value from a generator
whilst retaining the current location so that execution
may be resumed at the point after the yield.

For this to work a closure of the generator must be stored
in a variable which is subsequently applied.

.. code-block:: felix
   
   gen counter () = { 
     var x = 0;
   next_integer:>
     yield x;
     ++x;
     goto next_integer;
   }
   
   var counter1 = counter;
   var zero = counter1 ();
   var one = counter1 ();
   println$ zero, one;


Conditional Branches
--------------------

match/endmatch
^^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/grammar/patterns.flxh>`_

The match statement is used to match a value against
one or more structural patterns in sequence and perform
the handler code for the first matching pattern.

Patterns may specify pattern variables which are set
to the corresponding subpart of the value.

Patterns can match any type with a literal value
and equality operator, tuples, records, sum types
or union constructors.

A boolean subcomponent may be matched against
the special pattern words ``true`` and ``false``.

Patterns match recursively, drilling down into
the subcomponents of a term.  A subpart of a match
succeeds when a comparison with a literal for equality
returns true or the pattern has a variable or wildcard.

It fails when a comparison with a literal for equality
returns false, or a union or sum constructor does
not agree with the runtime value of the variant.

A record will match a pattern denoting a record
with a subset of the fields.

For polymorphic union the type variables
are not required, since they can be deduced.

For sum matches, the sum type is not required
as it can be deduced.

If none of the patterns in a match succeeds an
exception is thrown and the program terminates.

Pattern variables are identifiers preceded by a ``?``
mark. 

At most one occurence of a particular variable is allowed
in a pattern.

Plain identifiers must be union constructor
names.

The underscore symbol ``_`` is a wildcard match,
it is equivalent to a fresh pattern variable which is 
never used.

Disjunctions of patterns (alternatives) can be formed
with the ``|`` combinator.

A pattern may be suffixed by a ``when`` clause the
argument of which must be a boolean expression which
acts as a guard, and must evaluate true for the
pattern as a whole to match. The clause may contain
pattern variables which are in scope.

An expression in parentheses preceded by a ``$`` sign
is a short hand for a fresh pattern variable followed
by a when clause containing the term inside the parentheses.

If a type has both equality and a less than operator ``<``
it can be matched against an inclusive range specified by
two literals separated by ``..``.

A subpart of a match can be assigned to a pattern variable
with an ``as`` suffix.

.. code-block:: felix

   // integer match
   match a with
   | 0 => println$ "zero";
   | 1 => println$ "one";
   | ?x => println$ "Other: " + x.str;
   endmatch;

   // integer range match
   match a with
   | 0 .. 20 => println$ "Young";
   | _ => println$ "old";
   endmatch;

   // tuple match
   match a with
   | ?n,0 => println$ "Divide by zero";
   | 0,?d => println "zero"
   | ?n,?d => println "Quotient " + (n/d).str
   endmatch;

   // union match
   match a with
   | Some ?x => println$ "Got " + x.str;
   | None => println$ "Got nothing";
   endmatch; 

   // Sum match
   match case 0 of (int + string)  with
   | case 0 ?x => println "Int " + x.str;
   | case 1 ?x => println "String " + x;
   endmatch;

   // record match
   match (a=1, b=2, c=3) with
   | (b=42) => println$ "b is 42";
   | (a=?x, c=?z) => println$ "a,b=" + x.str + "," + y.str;
   endmatch;

  // when clause
  match 42 with
  | ?x when x < 20 => println$ x.str is " a teenie";
  | _ => println$ "Old hat";
  endmatch;

  var hitch = 42;
  match 42 with
  | $(hitch) => println$ "The Answer";
  | _ => println$ "Unknown";
  endmatch;

  // match with alternatives
  match a with
  | (1|2) => println$ "Buckle my shoe";
  | _ => println$ "open the door";
  endmatch;

 
  // match with as clause
  match a with
  | ((1,?x) as y),?z => println$ y.str;
  endmatch;


if/goto
^^^^^^^

The conditional goto is an abbreviation for 
the more verbose conditional:

.. code-block:: felix
   
   if c goto lab; // equivalent to
   if c do goto lab; done

if/return
^^^^^^^^^

The conditional return is an abbreviation for
the more verbose conditional:

.. code-block:: felix
   
   if c return; // equivalent to
   if c do return; done

if/call
^^^^^^^

The conditional call is an abbreviation for
the more verbose conditional:

.. code-block:: felix
   
   if c call f x; // equivalent to
   if c do call f x; done


if/do/elif/else/done
--------------------

The procedural conditional branch is used to select
a control path based on a boolean expression.

The ``else`` and ``elif`` clauses are optional.

.. code-block:: felix

   if c1 do 
     stmt1;
     stmt2;
   elif c2 do
     stmt3;
     stmt4;
   else
     stmt5;
     stmt6;
   done

The ``elif`` clause saves writing a nested conditional.
The above is equivalent to:

.. code-block:: felix
   
   if c1 do 
     stmt1;
     stmt2;
   else 
     if c2 do
       stmt3;
       stmt4;
     else
       stmt5;
       stmt6;
     done
   done

One or more statements may be givn in the selected control path.

A simple conditional is an abbreviation for a statement match:

.. code-block:: felix
   
   if c do stmt1; stmt2; else stmt3; stmt4; done
   // is equivalent to
   match c with
   | true => stmt1; stmt2; 
   | false => stmt3; stmt4;
   endmatch;


Loops
-----

`Reference <http://felix-lang.org/share/lib/grammar/loops.flxh>`_

Felix has some low level and high level loop constructions.

The low level for, while, and repeat loops are equivalent
to loops implemented with gotos.

The bodies of do loops do not constitute a scope,
therefore any symbol defined in such a body is also visible
in the surrounding code.

Low level loops may be labelled with a loop label
which is used to allow break, continue, and redo
statements to exit from any containing loop.

.. code-block:: felix
   
   outer:for var i in 0 upto 9 do
      inner: for var j in 0 upto 9 do
        println$ i,j;
        if i == j do break inner; done
        if i * j > 60 do break outer; done
      done
   done


redo
^^^^

The redo statement causes control to jump to the start
of the specified loop without incrementing the control variable.

break
^^^^^

The break statement causes control to jump past the end of
the specified loop, terminating iteration.

continue
^^^^^^^^

The continue statement causes the control variable to
be incremented and tests and the next iteration commenced
or the loop terminated.

for/in/upto/downto/do/done
^^^^^^^^^^^^^^^^^^^^^^^^^^

A basic loop with an inclusive range.

.. code-block:: felix
   
   // up
   for var ti:int in 0 upto 9 do println$ ti; done
   for var i in 0 upto 9 do println$ i; done
   for i in  0 upto 9 do println$ i; done
   
   // down
   for var tj:int in 9 downto 0 do println$ j; done
   for var j in 9 downto 0 do println$ j; done
   for j in  0 upto 9 do println$ j; done

The start and end expressions must be of the same type.

If the control variable is defined in the loop with a type
annotation, that type must agree with the control variable.

The type must support comparison with the equality operator ``==``
the less than or equals operator ``<=`` and increment with 
the pre increment procedure ``++``.

For loops over unsigned types cannot handle the empty case.
For loops over signed types cannot span the whole range of the type.

The loop logic takes care to ensure the control variable is not
incremented (resp. decremented) past the end (resp.start) value.

while/do/done
^^^^^^^^^^^^^

The while loop executes the body repeatedly whilst the control
condition is true at the start of the loop body.

.. code-block:: felix
   
   var i = 0;
   while i < 10 do println$ i; ++i; done

until loop
^^^^^^^^^^

The until loop executes the loop body repeatedly
until the control condition is false at the start of the loop,
it is equivalent o a while loop with a negated condition.

.. code-block:: felix
   
   var i = 0;
   until i == 9 do println$ i; ++i; done

for/match/done
^^^^^^^^^^^^^^

TBD

Fibres
------

spawn_fthread
^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/fibres.flx>`_

The ``spawn_fthread`` library function invokes the corresponding
service call to schedule the initial continuation of a procedure 
taking a unit argument as an fthread (fibre). 

The spawned fthread begins executing immediately.
If coutrol returns before yielding by a synchronous
channel operation, the action is equivalent to calling
the procedure.

Otherwise the spawned fthread is suspended when the first
write, or the first unmatched read operation occurs.


read/write/broadcast schannel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/schannels.flx>`_

Pthread
-------

spawn_pthread
^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/pthread.flx>`_

read/write pchannel
^^^^^^^^^^^^^^^^^^^

`Reference <http://felix-lang.org/share/lib/std/control/pchannels.flx>`_


Service call
------------

The service call statement calls the Felix system kernel
to perform a specified operation.

It is equivalent to an OS kernel call.

The available operations include:

.. code-block:: felix
   
     union svc_req_t =
     /*0*/ | svc_yield
     /*1*/ | svc_get_fthread         of &fthread    // CHANGED LAYOUT
     /*2*/ | svc_read                of address
     /*3*/ | svc_general             of &address    // CHANGED LAYOUT
     /*4*/ | svc_reserved1
     /*5*/ | svc_spawn_pthread       of fthread
     /*6*/ | svc_spawn_detached      of fthread
     /*7*/ | svc_sread               of _schannel * &gcaddress
     /*8*/ | svc_swrite              of _schannel * &gcaddress
     /*9*/ | svc_kill                of fthread
     /*10*/ | svc_reserved2
     /*11*/ | svc_multi_swrite       of _schannel * &gcaddress 
     /*12*/ | svc_schedule_detached  of fthread
     ;

These operations are typically related to coroutine or thread scheduling.
However ``svc_general`` is an unspecified operation, which is typically
used to invoke the asynchronous I/O subsystem.

Service calls can only be issued from flat code, that is,
from procedures, since they call the system by returning
control, the system must reside exactly one return address
up the machine stack at the point a service call is executed.

with/do/done
------------

The with/do/done statement is use to define temporary variables
which are accessible only in the do/done body of the statement.

It is the statement equivalent of the let expression.

.. code-block:: felix
   
   var x = 1;
   with var x = 2; do println$ x; done
   assert x == 1;

do/done
-------

The do/done statement has no semantics and merely acts as a
way to make a sequence of statements appear as a single
statement to the parser.

Jumps into do/done groups are therefore allowed, and
any labels defined in a do/done group are visible in
the enclosing context.

Any variables, functions, or other symbols defined in a do/done
group are visible in the enclosing context.

.. code-block:: felix
   
   do something; done

begin/end
---------

The begin/end statement creates an anonymous procedure
and then calls it. It therefore appears as a single statement
to the parser, but it simulates a block as would be used in C.
It is exactly equivalent to a brace enclosed procedure called
by a terminating semi-colon.

.. code-block:: felix
   
   begin
     var x = 1;
   end
   // equivalent to
   {
     var x = 1;
   };


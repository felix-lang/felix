Logical Operations
==================

Felix provides two boolean types: :code:`bool` and :code:`cbool`
with constants :code:`true` and :code:`false` and
:code:`ctrue` and :code:`cfalse` respectively and
the following basic operations:

========== ======== =========
Operator   Function Semantics
========== ======== =========
or         lor      disjunction
orelse     orelse   lazy disjunction
           nor      negated disjunction
and        land     conjunction
andthen    andthen  lazy conjunction
           nand     negated conjunction
not        not      negation
implies    implies  implication
========== ======== =========

The lazy forms require a function of type
:code:`1->bool` and :code:`1->cbool` respectively
and their second argument, which is evaluated only if the 
first argument is false.

The type bool is an alias for the sum type 2,
which is a compact linear type and will cost 64bits of store.

The type cbool is a binding to C++ bool, and is typically
one byte. Functionally these types are equivalent however
pointers to cbool are sometimes required for C++ compatibility.


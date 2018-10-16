Identifiers
===========

Felix has a rich lexicology for identifiers.

C like identifiers
------------------

C like identifiers start with a letter, and are optionally
followed by a sequence of letters, underscores, single quotes,
hyphens, or ASCII digits. 

The first character may also be an underscore
but that is reserved for the system. 

A letter may be any Unicode code point accepted by ISO C++ Standard
as a letter, it must be encoded as UTF-8.

.. code-block:: felix

    Hello
    X123
    a'
    julie-marx
    xИΫຜشஇᄗデՖ

Note again please, only ASCII digits are permitted.


Tex Identifiers
---------------

A leading slosh followed by a nonempty sequence of ASCII letters
is recognised as an identifer. With suitable output
machinery, the corresponding LaTeX or AmSTeX symbol should display
if there is one.

.. code-block:: felix

    \alpha

displays as

.. code-block:: xfelix

    \alpha

No space is required after a TeX identifier. Therefore

.. code-block:: felix

    \alpha2

encodes the symbol \alpha followed by the integer 2.


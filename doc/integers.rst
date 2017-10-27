Integer Types
=============

Felix supports several families of integer types.


Integers Types
--------------

Platform variant integer types are based on C integer types 
whose sizes vary from platform to platform. In the tables below,
the suffix is appended to a plain integer literal as a type modifier.
The suffix letters can be in any order and can be upper or lower case.
 

ISO C Signed Integer Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

The size in bytes on Windows 32 and 64 bit platforms and
Unix 32 and 64 bit platfoms is indicated.

========== =============== =======  === === === ===
Felix       C              Suffix   W32 W64 U32 U64
========== =============== =======  === === === ===
tiny       signed char     t        1   1   1   1      
short      short           s        2   2   2   2
int        int                      2   2   4   4
long       long            l        4   4   4   8
vlong      long long       vl,lv    8   8   8   8
========== =============== =======  === === === ===

Int type may also use `i` suffix.

ISO C Unsigned Integer Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All unsigned integer types provide a range of values
from 0 to $256^n-1$ for some n. The size in bytes
is the same as the corresponding signed type.

==========  =================== ========
Felix       C                   Suffix
==========  =================== ========
utiny       usigned char        ut
ushort      unsigned short      us
uint        unsigned int        u
ulong       unsigned long       ul
uvlong      unsigned long long  uvl,ulv
==========  =================== ========

ISO C and Posix Aliases
~~~~~~~~~~~~~~~~~~~~~~~

==========  =================== =========
Felix       C                   Suffix
==========  =================== =========
intmax      intmax_t            j
uintmax     uintmax_t           uj
ssize       ssize_t             z
size        size_t              uz
intptr      intptr_t            p
uintptr     uintptr_t           up
ptrdiff     ptrdiff_t           d
uptrdiff    uptrdiff_t          ud
==========  =================== =========

The intmax and uintmax types are 8 bytes on Windows and Unix platforms.
The other types are all the same as the machine word size,
either 4 or 8 bytes.


Exact Signed Integer Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

==========  =================== ========
Felix       C                   Suffix
==========  =================== ========
int8        int8_t              i8
int16       int16_t             i16
int32       int32_t             i32
int64       int64_t             i64
==========  =================== ========


Exact Unigned Integer Types
~~~~~~~~~~~~~~~~~~~~~~~~~~

==========  =================== ==========
Felix       C                   Suffix
==========  =================== ==========
uint8       uint8_t             u8
uint16      uint16_t            u16
uint32      uint32_t            u32
uint64      uint64_t            u64
==========  =================== ==========

Integer Literals
----------------

An integer literal consists of an optional radix indicator,
a string of digits with possible embedded spacers, and an
optional suffix.


Radices
-------

Felix supports 4 radices.

========= ======   =========================
Radix     Prefix   Digits
========= ======   =========================
Hex       0x       0123456789abcdefABCDEF
Decimal   0d       0123456789
Octal     0o       01234567
Binary    0b       01
========= ======   =========================

If omitted decimal radix if used.
Radix letter may be upper or lower case.
Note, in Felix a leading zero digit does not imply octal
radix as in C.


Spacers
~~~~~~~

Integers allow an underscore between digits, after
the radix specifier if one is given, or before the
suffix specifier, if one is given.

Operations
----------

All Integers
~~~~~~~~~~~~

All integer types support the following operations.

===========       =============
Operation         Operator
===========       =============
Addition          infix +
Subtraction       infix -
Multiplication    infix *
Division          infix /
Remainder         infix %
Left Shift        infix <<
Right Shift       infix >>
===========       =============

Left and right shifts are defined as multiplication by positive
or negative powers of 2, respectively.

Signed Integers Only
~~~~~~~~~~~~~~~~~~~~

===========    ==================
Operation      Operator
===========    ==================
Negation       prefix -, neg
Sign           sgn
Absolute Value abs
===========    ==================

The `sgn` operator returns -1 for negative, 0 for zero, and 1
for positive.


Unsigned Integer Only
~~~~~~~~~~~~~~~~~~~~~

These operations are bitwise logic operations.
They are not available for signed integers.

================   ===========
Operation          Operator
================   ===========
ones complement    ~ 
bitand             infix \&
bitor              infix \|
bitxor             infix \^
================   ===========




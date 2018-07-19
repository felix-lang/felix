Address Types
=============

Several special types are provided for raw memory manipulation:
`caddress`, `address` and `byte`.

An address is a machine address which can be treated as an
unsigned integer, and also supports the dereference operator.
All standard pointers can be converted to type `address`
and `caddress`.


The dereference function `deref` which can also be written
with a prefix `*` as in C, returns a value of type `byte`.
A byte is an 8 bit unsigned integer similar to `uint8`.

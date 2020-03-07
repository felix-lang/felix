# uint256_t

An unsigned 256 bit integer type for C++

Copyright (c) 2013 - 2018 Jason Lee @ calccrypto at gmail.com

Please see LICENSE file for license.

[![Build Status](https://travis-ci.org/calccrypto/uint256_t.svg?branch=master)](https://travis-ci.org/calccrypto/uint256_t)

## Acknowledgements
With much help from Auston Sterling

Thanks to Stefan Deigmüller for finding
a bug in operator*.

Thanks to François Dessenne for convincing me
to do a general rewrite of this class.

Thanks to John Skaller for making symbols visible
when compiling as a shared library.

## Usage
This is simple implementation of an unsigned 256 bit
integer type in C++. It's meant to be used like a standard
`uintX_t`, except with a larger bit size than those provided
by C/C++.

`uint256_t` requires [`uint128_t`](https://github.com/calccrypto/uint128_t), which is included.

### In Code
All that needs to be done in code is `#include "uint256_t.h"`

```c++
#include <iostream>
#include "uint256_t.h"

int main() {
    uint256_t a = 1;
    uint256_t b = 2;
    std::cout << (a | b) << std::endl;
    return 0;
}
```

### Compilation
A C++ compiler supporting at least C++11 is required.

Compilation can be done by directly including `uint128_t.cpp` and `uint256_t.cpp` in your compile command, e.g. `g++ -std=c++11 main.cpp uint128_t.cpp uint256_t.cpp`, or other ways, such as linking the `uint128_t.o` and `uint256_t.o` files, or creating a library, and linking the library in.

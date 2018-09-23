# integer
### A C++ Arbitrary Precision Integer Implementation
Copyright (c) 2013 - 2017 Jason Lee @ calccrypto at gmail.com

Please see LICENSE file for license.

[![Build Status](https://travis-ci.org/calccrypto/integer.svg?branch=master)](https://travis-ci.org/calccrypto/integer)

With much help from:
- Auston Sterling - Initial debugging and coding help and FFT multiplication
- Corbin @ Code Review (StackExchange) - wrote a sizeable chunk of code and suggestions
- Keith Nicholas @ Code Review (StackExchange)
- mugwort-rc - tons of edits
- ROBOKITTY @ Code Review (StackExchange)
- Winston Ewert @ Code Review (StackExchange) - suggested many improvements

Although this class can theoretically hold an infinitely large value,
the actual limit of how large the integers can be is
`std::deque <Z>().max_size() * sizeof(Z) * 8 bits`, which
should be more than enough for any purpose.

#### Usage:
    #include <iostream>
    #include "integer.h"

    int main(){
        integer a = -1234;         // standard integer input
        integer b(  "5678",  16);  // string input (no base prefixes: '0b' or '0x')
        integer c( "-9abC",  16);  // hex input cases can be mixed
        integer d("Hello!", 256);  // ASCII strings can be used too!

        integer sum = a + b + c + d + 1;           // 79600447923724 (decimal)

        std::cout << sum             << std::endl; // print directly
        std::cout << sum.str(16)     << std::endl; // print with a specific base
        std::cout << std::hex << sum << std::endl; // print using std::oct, std::dec, and std::hex

        return 0;
    }

#### Compilation
`c++ <some arguments> integer.cpp <other arguments>`
- g++ and clang++ should both work
- C++11 is required

#### Internal Operations
- Data is stored in big-endian, so value[0] is the most
  significant digit, and value[value.size() - 1] is the
  least significant digit.
    - By default, the container holding the value is `std::deque <uint8_t>`.

    - Changing the internal representation to a std::string
      makes integer run slower than using a `std::deque <uint8_t>`

- Negative values are stored as their positive value,
  with a bool that says the value is negative.

- Arithmetic (+, -, *, /, %) and Comparison (>, >=, <, <=, ==, !=) operators
  operate on the absolute values of the operands from within private functions.
  Signs are handled in the operators themselves.

  - Multiple algorithms for subtraction, multiplication, and
    division have been implemented and commented out. They
    should all work, but are there for educational purposes.
    If one is faster than another on different systems, by
    all means change which algorithm is used. Just make sure
    all related functions are changed as well.

- Bitwise operations on negative values will use the
  two's complement version of the number.

#### Notes

- Conversions for strings of values in bases [2, 16] and 256 are provided.
  If others are required, add the conversions to
  `integer::integer(Iterator, const Iterator&, const uint16_t &)`
  and
  `std::string integer::str(const uint16_t &, const unsigned int &) const`

- The constructor using iterators allows for creating values
  from any integral base larger than 1.

- Hexadecimal output strings use lowercase characters.

- Base256 strings are assumed to be positive when read into
  integer. Use operator-() to negate the value.

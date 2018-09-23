#include "integer.build"

constexpr INTEGER_DIGIT_T integer::NEG1;
constexpr std::size_t     integer::OCTETS;
constexpr std::size_t     integer::BITS;
constexpr INTEGER_DIGIT_T integer::HIGH_BIT;
constexpr integer::Sign   integer::POSITIVE;
constexpr integer::Sign   integer::NEGATIVE;

integer & integer::trim(){                  // remove top 0 digits to save memory
    while (!_value.empty() && !_value[0]){
        _value.pop_front();
    }
    if (_value.empty()){                    // change sign to false if _value is 0
        _sign = integer::POSITIVE;
    }

    return *this;
}

// Constructors
integer::integer() :
    _sign(integer::POSITIVE),
    _value()
{}

integer::integer(const integer & copy) :
    _sign(copy._sign),
    _value(copy._value)
{
    trim();
}

integer::integer(integer && copy) :
    _sign(std::move(copy._sign)),
    _value(std::move(copy._value))
{
    copy = 0;
    trim();
}

integer::integer(const integer::REP & rhs, const integer::Sign & sign) :
    _sign(sign),
    _value(rhs)
{
    trim();
}

integer::integer(const bool & b) :
    _sign(false),
    _value(1, b)
{
    trim();
}

// Special Constructor for Strings
// bases 2-16 and 256 are allowed
//      Written by Corbin http://codereview.stackexchange.com/a/13452
//      Modified by me
integer::integer(const std::string & str, const integer & base) : integer()
{
    if ((2 <= base) && (base <= 16)){
        if (!str.size()){
            return;
        }

        integer::Sign sign = integer::POSITIVE;

        std::string::size_type index = 0;

        // minus sign indicates negative value
        if (str[0] == '-'){
            // make sure there are more digits
            if (str.size() < 2){
                throw std::runtime_error("Error: Input string is too short");
            }

            sign = integer::NEGATIVE;
            index++;
        }

        // process characters
        for(; index < str.size(); index++){
            uint8_t d = std::tolower(str[index]);
            if (std::isdigit(d)){       // 0-9
                d -= '0';
                if (d >= base){
                    throw std::runtime_error(std::string("Error: Not a digit in base ") + base.str(10) + ": '"+ str[index] + "'");
                }
            }
            else if (std::isxdigit(d)){ // a-f
                d -= 'a' - 10;
                if (d >= base){
                    throw std::runtime_error(std::string("Error: Not a digit in base ") + base.str(10) + ": '"+ str[index] + "'");
                }
            }
            else{                       // bad character
                throw std::runtime_error(std::string("Error: Not a digit in base ") + base.str(10) + ": '"+ str[index] + "'");
            }

            *this = (*this * base) + d;
        }

        _sign = sign;
    }
    else if (base == 256){
        // process characters
        for(unsigned char const & c : str){
            *this = (*this << 8) | (c & 0xff);
        }
    }
    else{
        throw std::runtime_error("Error: Cannot convert from base " + base.str(10));
    }

    trim();
}

//  RHS input args only

// Assignment Operators
integer & integer::operator=(const integer & rhs){
    _sign = rhs._sign;
    _value = rhs._value;
    return trim();
}

integer & integer::operator=(integer && rhs){
    if (*this != rhs){
        _sign = rhs._sign;
        _value = rhs._value;
        rhs = 0;
    }
    return trim();
}

// Typecast Operators
integer::operator bool() const {
    return !_value.empty();
}

integer::operator uint8_t() const {
    const uint8_t out = static_cast <uint8_t> (_value.empty()?0:_value.back() & 255);
    return _sign?-out:out;
}

integer::operator uint16_t() const {
    uint16_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 2 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <uint16_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

integer::operator uint32_t() const {
    uint32_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 4 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <uint32_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

integer::operator uint64_t() const {
    uint64_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 8 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <uint64_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

integer::operator int8_t() const {
    const int8_t out = static_cast <int8_t> (_value.empty()?0:_value.back() & 255);
    return _sign?-out:out;
}

integer::operator int16_t() const {
    int16_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 2 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <int16_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

integer::operator int32_t() const {
    int32_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 4 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <int32_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

integer::operator int64_t() const {
    int64_t out = 0;

    const integer::REP_SIZE_T d = std::min(digits(), std::max((integer::REP_SIZE_T) 8 / integer::OCTETS, (integer::REP_SIZE_T) 1));
    for(integer::REP_SIZE_T x = 0; x < d; x++){
        out += static_cast <int64_t> (_value[digits() - x - 1]) << (x * integer::BITS);
    }

    return _sign?-out:out;
}

// Bitwise Operators
integer integer::operator&(const integer & rhs) const {
    integer::REP out;

    const integer::REP_SIZE_T max_bits = std::max(bits(), rhs.bits());
    const integer             left     = (    _sign == integer::POSITIVE)?*this:twos_complement(max_bits);
    const integer             right    = (rhs._sign == integer::POSITIVE)?rhs:rhs.twos_complement(max_bits);

    // AND matching digits
    for(integer::REP::const_reverse_iterator i = left._value.rbegin(), j = right._value.rbegin(); (i != left._value.rend()) && (j != right._value.rend()); i++, j++){
        out.push_front(*i & *j);
    }

    // drop any digits that don't match up

    integer OUT(out, integer::POSITIVE);
    if (_sign & rhs._sign){
        OUT = OUT.twos_complement(max_bits);
    }

    return OUT.trim();
}

integer & integer::operator&=(const integer & rhs){
    return *this = *this & rhs;
}

integer integer::operator|(const integer & rhs) const {
    const integer::REP_SIZE_T max_bits = std::max(bits(), rhs.bits());
    const integer             left     = (    _sign == integer::POSITIVE)?*this:twos_complement(max_bits);
    const integer             right    = (rhs._sign == integer::POSITIVE)?rhs:rhs.twos_complement(max_bits);

    integer::REP out;
    integer::REP::const_reverse_iterator i = left._value.rbegin(), j = right._value.rbegin();

    // OR matching digits
    for(; (i != left._value.rend()) && (j != right._value.rend()); i++, j++){
        out.push_front(*i | *j);
    }

    // push rest of *this into value
    while (i != left._value.rend()){
        out.push_front(*i++);
    }

    // push rest of rhs into value
    while (j != right._value.rend()){
        out.push_front(*j++);
    }

    integer OUT(out, integer::POSITIVE);
    if (_sign | rhs._sign){
        OUT = OUT.twos_complement(max_bits);
    }

    return OUT.trim();
}

integer & integer::operator|=(const integer & rhs){
    return *this = *this | rhs;
}

integer integer::operator^(const integer & rhs) const {
    const integer::REP_SIZE_T max_bits = std::max(bits(), rhs.bits());
    const integer             left     = (    _sign == integer::POSITIVE)?*this:twos_complement(max_bits);
    const integer             right    = (rhs._sign == integer::POSITIVE)?rhs:rhs.twos_complement(max_bits);

    integer::REP out;
    integer::REP::const_reverse_iterator i = left._value.rbegin(), j = right._value.rbegin();

    // XOR matching digits
    for(; (i != left._value.rend()) && (j != right._value.rend()); i++, j++){
        out.push_front(*i ^ *j);
    }

    // push *this into value
    while (i != left._value.rend()){
        out.push_front(*i++);
    }

    // push rhs into value
    while (j != right._value.rend()){
        out.push_front(*j++);
    }

    integer OUT(out, integer::POSITIVE);
    if (_sign ^ rhs._sign){
        OUT = OUT.twos_complement(max_bits);
    }

    return OUT.trim();
}

integer & integer::operator^=(const integer & rhs){
    return *this = *this ^ rhs;
}

integer integer::operator~() const {
    // in case value is 0
    if (_value.empty()){
        return 1;
    }

    integer::REP out = _value;

    // invert whole digits
    for(integer::REP_SIZE_T i = 1; i < out.size(); i++){
        out[i] ^= integer::NEG1;
    }

    INTEGER_DIGIT_T mask = HIGH_BIT;
    while (!(out[0] & mask)){
        mask >>= 1;
    }

    // invert bits of partial digit
    while (mask){
        out[0] ^= mask;
        mask >>= 1;
    }

    return integer(out, _sign);
}

// Bit Shift Operators

// left bit shift. sign is maintained
integer integer::operator<<(const integer & shift) const {
    if (!*this || !shift){
        return *this;
    }

    if (shift < 0){
        throw std::runtime_error("Error: Negative shift amount");
    }

    const std::pair <integer, integer> qr = dm(shift, integer::BITS);
    const integer & whole      = qr.first;             // number of zeros to add to the back
    const INTEGER_DIGIT_T push = qr.second;            // push left by this many bits
    const INTEGER_DIGIT_T pull = integer::BITS - push; // pull "push" bits from the right

    integer::REP out = _value;

    out.push_front(0);                                 // extra digit for shifting into
    out.push_back(0);                                  // extra digit for shifting from

    // do this part first to avoid shifting zeros
    for(integer::REP_SIZE_T i = 0; i < (out.size() - 1); i++){
        INTEGER_DOUBLE_DIGIT_T d = out[i];
        d = (d << push) | (out[i + 1] >> pull);
        out[i] = d & NEG1;
        // out[i] = (out[i] << push) | (out[i + 1] >> pull);
    }

    if (!out[0]){                                      // if the top digit is still 0
        out.pop_front();                               // remove it
    }

    if (!whole){                                       // if there was no need for the 0 at the end
        out.pop_back();                                // remove it
    }
    else{
        // push back zeros, excluding the one already there
        out.insert(out.end(), whole - 1, 0);
    }

    return integer(out, _sign);
}

integer & integer::operator<<=(const integer & shift){
    return *this = *this << integer(shift);
}

// right bit shift. sign is maintained
integer integer::operator>>(const integer & shift) const {
    if (shift < 0){
        throw std::runtime_error("Error: Negative shift amount");
    }

    if (shift >= bits()){
        return 0;
    }

    const std::pair <integer, integer> qr = dm(shift, integer::BITS);
    const integer & whole      = qr.first;             // number of digits to pop off
    const INTEGER_DIGIT_T push = qr.second;            // push right by this many bits
    const INTEGER_DIGIT_T pull = integer::BITS - push; // pull "push" bits from the left

    integer::REP out = _value;

    // pop off whole digits
    for(integer i = 0; i < whole; i++){
        out.pop_back();
    }

    if (push){
        out.push_front(0);                             // extra 0 for shifting from
        for(integer::REP_SIZE_T i = 1; i < out.size(); i++){
            out[out.size() - i] = (out[out.size() - i - 1] << pull) | (out[out.size() - i] >> push);
        }
        out.pop_front();
    }

    return integer(out, _sign);
}

integer & integer::operator>>=(const integer & shift){
    return *this = *this >> integer(shift);
}

// Logical Operators
bool integer::operator!(){
    return !static_cast <bool> (*this);
}

// Comparison Operators
bool integer::operator==(const integer & rhs) const {
    return ((_sign == rhs._sign) && (_value == rhs._value));
}

bool integer::operator!=(const integer & rhs) const {
    return !(*this == rhs);
}

// operator> not considering signs
bool integer::gt(const integer & lhs, const integer & rhs) const {
    if (lhs._value.size() > rhs._value.size()){
        return true;
    }
    if (lhs._value.size() < rhs._value.size()){
        return false;
    }
    if (lhs._value == rhs._value){
        return false;
    }
    for(integer::REP_SIZE_T i = 0; i < lhs._value.size(); i++){
        if (lhs._value[i] != rhs._value[i]){
            return lhs._value[i] > rhs._value[i];
        }
    }
    return false;
}

bool integer::operator>(const integer & rhs) const {
    if      (    (_sign == integer::NEGATIVE) &&    // - > +
             (rhs._sign == integer::POSITIVE)){
        return false;
    }
    else if (   (_sign == integer::POSITIVE) &&     // + > -
            (rhs._sign == integer::NEGATIVE)){
        return true;
    }
    else if (   (_sign == integer::NEGATIVE) &&     // - > -
            (rhs._sign == integer::NEGATIVE)){
        return gt(rhs, *this);
    }
    // else if (    (_sign == integer::POSITIVE) && // + > +
            //  (rhs._sign == integer::POSITIVE)){
    return gt(*this, rhs);
}

bool integer::operator>=(const integer & rhs) const {
    return ((*this > rhs) | (*this == rhs));
}

// operator< not considering signs
bool integer::lt(const integer & lhs, const integer & rhs) const {
    if (lhs._value.size() < rhs._value.size()){
        return true;
    }
    if (lhs._value.size() > rhs._value.size()){
        return false;
    }
    if (lhs._value == rhs._value){
        return false;
    }
    for(integer::REP_SIZE_T i = 0; i < lhs._value.size(); i++){
        if (lhs._value[i] != rhs._value[i]){
            return lhs._value[i] < rhs._value[i];
        }
    }
    return false;
}

bool integer::operator<(const integer & rhs) const {
    if      (    (_sign == integer::NEGATIVE) &&     // - < +
             (rhs._sign == integer::POSITIVE)){
        return true;
    }
    else if (    (_sign == integer::POSITIVE) &&     // + < -
             (rhs._sign == integer::NEGATIVE)){
        return false;
    }
    else if (    (_sign == integer::NEGATIVE) &&     // - < -
             (rhs._sign == integer::NEGATIVE)){
        return lt(rhs, *this);
    }
    // else if (    (_sign == integer::POSITIVE) &&  // + < +
            //  (rhs._sign == integer::POSITIVE)){
    return lt(*this, rhs);
}

bool integer::operator<=(const integer & rhs) const {
    return ((*this < rhs) | (*this == rhs));
}

// Arithmetic Operators
integer integer::add(const integer & lhs, const integer & rhs) const {
    integer::REP out;
    integer::REP::const_reverse_iterator i = lhs._value.rbegin(), j = rhs._value.rbegin();
    bool carry = false;
    INTEGER_DOUBLE_DIGIT_T sum;

    // add up matching digits
    for(; ((i != lhs._value.rend()) && (j != rhs._value.rend())); i++, j++){
        sum = static_cast <INTEGER_DOUBLE_DIGIT_T> (*i) + static_cast <INTEGER_DOUBLE_DIGIT_T> (*j) + carry;
        out.push_front(sum);
        carry = (sum > integer::NEG1);
    }

    // copy in lhs extra digits
    for(; i != lhs._value.rend(); i++){
        sum = static_cast <INTEGER_DOUBLE_DIGIT_T> (*i) + carry;
        out.push_front(sum);
        carry = (sum > integer::NEG1);
    }

    // copy in rhs extra digits
    for(; j != rhs._value.rend(); j++){
        sum = static_cast <INTEGER_DOUBLE_DIGIT_T> (*j) + carry;
        out.push_front(sum);
        carry = (sum > integer::NEG1);
    }

    if (carry){
        out.push_front(1);
    }
    return integer(out);
}

integer integer::operator+(const integer & rhs) const {
    if (!rhs){
        return *this;
    }

    if (!*this){
        return rhs;
    }

    integer out = *this;
    if (gt(out, rhs)){              // lhs > rhs
        if (_sign == rhs._sign){    // same sign: lhs + rhs
            out = add(out, rhs);
        }
        else{                       // different signs: lhs - rhs
            out = sub(out, rhs);
        }
        out._sign = _sign;          // lhs sign dominates
    }
    else if (lt(out, rhs)){         // lhs < rhs
        if (_sign == rhs._sign){    // same sign: rhs + lhs
            out = add(rhs, out);
        }
        else{                       // different sign: rhs - lhs
            out = sub(rhs, out);
        }
        out._sign = rhs._sign;      // rhs sign dominates
    }
    else{                           // lhs == rhs
        if (_sign == rhs._sign){    // same sign: double value
            out <<= 1;
            out._sign = _sign;
        }
        else{                       // different signs: 0
            return 0;
        }
    }
    out.trim();
    return out;
}

integer & integer::operator+=(const integer & rhs){
    return *this = *this + rhs;
}

// Subtraction as done by hand
integer integer::long_sub(const integer & lhs, const integer & rhs) const {
    // rhs always smaller than lhs
    integer out = lhs;
    integer::REP_SIZE_T lsize = out._value.size() - 1;
    integer::REP_SIZE_T rsize = rhs._value.size() - 1;

    for(integer::REP_SIZE_T x = 0; x <= rsize; x++){
        // if top is bigger than or equal to the bottom, just substract
        if (out._value[lsize - x] >= rhs._value[rsize - x]){
            out._value[lsize - x] -= rhs._value[rsize - x];
        }
        else{// find a higher digit to carry from
            integer::REP_SIZE_T y = lsize - x - 1;
            // if this goes out of bounds, something is wrong
            while (!out._value[y]){
                y--;
            }

            out._value[y]--;
            y++;

            for(; y < lsize - x; y++){
                out._value[y] = integer::NEG1;
            }

            out._value[y] = static_cast <INTEGER_DOUBLE_DIGIT_T> (out._value[y]) + (static_cast <uint64_t> (1) << integer::BITS) - rhs._value[rsize - x];
        }
    }
    return out;
}

//// Two's Complement Subtraction
//integer integer::two_comp_sub(const integer & lhs, const integer & rhs){
//    rhs = rhs.twos_complement(lhs.bits());
//    return add(lhs, rhs) & (~(integer(1) << lhs.bits()));   // Flip bits to get max of 1 << x
//}

// subtraction not considering signs
// lhs must be larger than rhs
integer integer::sub(const integer & lhs, const integer & rhs) const {
    if (!rhs){
        return lhs;
    }
    if (!lhs){
        return -rhs;
    }
    if (lhs == rhs){
        return 0;
    }
    return long_sub(lhs, rhs);
    // return two_comp_sub(lhs, rhs);
}

integer integer::operator-(const integer & rhs) const {
    integer out = *this;
    if (gt(out, rhs)){                                  // if lhs > rhs
        if (out._sign == rhs._sign){                    // same signs
            out = sub(out, rhs);
        }
        else if (out._sign != rhs._sign){               // different signs
            out = add(out, rhs);
        }
        out._sign = _sign;                              // lhs sign dominates
    }
    else if (lt(out, rhs)){                             // if lhs < rhs
        if      (    (_sign == integer::NEGATIVE) &&    // - - -
                 (rhs._sign == integer::NEGATIVE)){
            out = sub(rhs, out);
            out._sign = integer::POSITIVE;
        }
        else if (    (_sign == integer::NEGATIVE) &&    // - - +
                 (rhs._sign == integer::POSITIVE)){
            out = add(rhs, out);
            out._sign = integer::NEGATIVE;
        }
        else if (    (_sign == integer::POSITIVE) &&    // + - -
                 (rhs._sign == integer::NEGATIVE)){
            out = add(out, rhs);
            out._sign = integer::POSITIVE;
        }
        else if (    (_sign == integer::POSITIVE) &&    // + - +
                 (rhs._sign == integer::POSITIVE)){
            out = sub(rhs, out);
            out._sign = integer::NEGATIVE;
        }
    }
    else{                                               // if lhs == rhs
        if (_sign == rhs._sign){                        // same signs: 0
            return 0;
        }
        else{                                           // different signs: double value
            out <<= 1;
            out._sign = _sign;
        }
    }
    out.trim();
    return out;
}

integer & integer::operator-=(const integer & rhs){
    return *this = *this - rhs;
}

// // Peasant Multiplication
// integer integer::peasant(const integer & lhs, const integer & rhs) const {
   // integer rhs_copy = rhs;
   // integer sum = 0;
   // for(integer::REP_SIZE_T x = 0; x < lhs.bits(); x++){
      // if (lhs[x]){
          // sum += add(sum, rhs_copy);
       // }
      // rhs_copy <<= 1;
   // }
   // return sum;
// }

// // Recurseive Peasant Algorithm
// integer integer::recursive_peasant(const integer & lhs, const integer & rhs) const {
   // if (!rhs){
       // return 0;
   // }
   // if (rhs & 1){
       // return lhs + recursive_peasant(lhs << 1, rhs >> 1);
   // }
   // return recursive_peasant(lhs << 1, rhs >> 1);
// }

// // Recursive Multiplication
// integer integer::recursive_mult(const integer & lhs, const integer & rhs) const {
   // if (!rhs){
      // return 0;
   // }
   // integer z = recursive_mult(lhs, rhs >> 1);
   // if (!(rhs & 1)){
      // return z << 1;
   // }
   // return add(lhs, z << 1);
// }

// // Karatsuba Algorithm
// integer integer::karatsuba(const integer & lhs, const integer & rhs, integer bm) const {
  // // b is integer::REP = 256
  // // m is chars = 4
  // // bm is max _value = b ^ m

  // if ((lhs <= bm) | (rhs <= bm))
      // return peasant(lhs, rhs);

  // std::pair <integer, integer> x = dm(lhs, bm);
  // std::pair <integer, integer> y = dm(rhs, bm);
  // integer x0 = x.second;
  // integer x1 = x.first;
  // integer y0 = y.second;
  // integer y1 = y.first;

  // integer z0 = karatsuba(x0, y0);
  // integer z2 = karatsuba(x1, y1);
  // integer z1 = sub(sub(karatsuba(add(x1, x0), add(y1, y0)), z2), z0);
  // return add(karatsuba(add(karatsuba(z2, bm), z1), bm), z0);
// }

// // Toom-Cook multiplication
// // as described at http://en.wikipedia.org/wiki/Toom%E2%80%93Cook_multiplications
// // This implementation is a bit weird. In the pointwise Multiplcation step, using
// // operator* and long_mult works, but everything else fails.
// // It's also kind of slow.
// integer integer::toom_cook_3(integer m, integer n, integer bm){
  // if ((m <= bm) | (n <= bm)){
      // return peasant(m, n);
   // }

  // // Splitting
  // integer i = integer(std::max(m.log(3), n.log(3))) / 3 + 1;
  // integer bi = pow(integer(3), i);
  // integer B = 1;
  // integer integer::REP = 10;
  // while (B < bi){
      // B *= integer::REP;
   // }

  // integer M[3], N[3];
  // for(uint8_t i = 0; i < 3; i++){
      // std::pair <integer, integer> tm = dm(m, B);
      // std::pair <integer, integer> tn = dm(n, B);
      // m = tm.first;
      // n = tn.first;
      // M[i] = tm.second;
      // N[i] = tn.second;
  // }

  // // Evaluation
  // //             {0,             1,                 -1,                         -2,                             inf}
  // integer p[5] = {M[0], M[0] + M[1] + M[2], M[0] - M[1] + M[2], M[0] - M[1] - M[1] + M[2] + M[2] + M[2] + M[2], M[2]};
  // integer q[5] = {N[0], N[0] + N[1] + N[2], N[0] - N[1] + N[2], N[0] - N[1] - N[1] + N[2] + N[2] + N[2] + N[2], N[2]};

  // // Pointwise Multiplication
  // integer r[5];
  // for(uint8_t i = 0; i < 5; i++)
      // r[i] = p[i] * q[i];                 // don't understand why only integer::operator* and long_mult can be used here

  // // Interpolation
  // integer r0 = r[0];
  // integer r4 = r[4];
  // integer r3 = (r[3] - r[1]) / 3;
  // integer r1 = (r[1] - r[2]) / 2;
  // integer r2 = r[2] - r[0];
  // r3 = (r2 - r3) / 2 + r4 + r4;
  // r2 = r2 + r1 - r4;
  // r1 = r1 - r3;

  // // Recomposition
  // return peasant(peasant(peasant(peasant(r4, B) + r3, B) + r2, B) + r1, B) + r0;
// }

// // Long multiplication
// integer integer::long_mult(const integer & lhs, const integer & rhs) const {
   // unsigned int zeros = 0;
   // integer row, out = 0;
   // for(integer::REP::const_reverse_iterator i = lhs._value.rbegin(); i != lhs._value.rend(); i++){
       // row._value = integer::REP(zeros++, 0); // zeros on the right hand side
       // INTEGER_DIGIT_T carry = 0;
       // for(integer::REP::const_reverse_iterator j = rhs._value.rbegin(); j != rhs._value.rend(); j++){
           // INTEGER_DOUBLE_DIGIT_T prod = (INTEGER_DOUBLE_DIGIT_T) *i * (INTEGER_DOUBLE_DIGIT_T) *j + carry;// multiply through
           // row._value.push_front(prod & integer::NEG1);
           // carry = prod >> integer::BITS;
       // }
       // if (carry){
           // row._value.push_front(carry);
       // }
       // out = add(out, row);
   // }
   // return out;
// }

//Private FFT helper function
int integer::fft(std::deque<double>& data, bool dir) const
{
     //Verify size is a power of two
     std::size_t n = data.size()/2;
     if ((n == 0) || (n & (n-1))) return 1;

     //rearrange data for signal flow chart
     std::size_t bitr_j = 1;
     for (std::size_t i = 3; i < 2*n-1; i += 2)
     {
          std::size_t msz = n;
          while (bitr_j >= msz)
          {
               bitr_j -= msz;
               msz >>= 1;
          }
          bitr_j += msz;

          if (bitr_j > i)
          {
               double swap = data[bitr_j-1];
               data[bitr_j-1] = data[i-1];
               data[i-1] = swap;
               swap = data[bitr_j];
               data[bitr_j] = data[i];
               data[i] = swap;
          }
     }

     //Perform "butterfly" calculations
     std::size_t lmax = 2;
     while (lmax <= n)
     {
          double wr = 1;
          double wi = 0;

          double theta = (2*M_PI)/double(lmax*(dir?1.0:-1.0));
          double wpr = cos(theta);
          double wpi = sin(theta);

          int pstep = 2*lmax;
          for (std::size_t l = 1; l < lmax; l += 2)
          {
               for (std::size_t p = l; p < 2*n; p += pstep)
               {
                    std::size_t q = p + lmax;
                    double tempr = wr*data[q-1] - wi*data[q];
                    double tempi = wr*data[q] + wi*data[q-1];
                    data[q-1] = data[p-1] - tempr;
                    data[q] = data[p] - tempi;
                    data[p-1] = data[p-1] + tempr;
                    data[p] = data[p] + tempi;
               }

               //Find the next power of W
               double wtemp = wr;
               wr = wr*wpr - wi*wpi;
               wi = wi*wpr + wtemp*wpi;
          }

          lmax = pstep;
     }

     //All is good
     return 0;
}

// FFT-based multiplication
//Based on the convolution theorem which states that the Fourier
//transform of a convolution is the pointwise product of their
//Fourier transforms.
integer integer::fft_mult(const integer& lhs, const integer& rhs) const {
     //Convert each integer to input wanted by fft()
     size_t size = 1;
     while (size < lhs._value.size()*2){
          size <<= 1;
     }
     while (size < rhs._value.size()*2){
          size <<= 1;
     }

     std::deque<double> lhs_fft;
     lhs_fft.resize(size*2, 0);
     for (size_t i = 0; i < lhs._value.size(); i++){
          lhs_fft[i*2] = double(lhs._value[lhs._value.size()-1-i]);
     }

     std::deque<double> rhs_fft;
     rhs_fft.resize(size*2, 0);
     for (size_t i = 0; i < rhs._value.size(); i++){
          rhs_fft[i*2] = double(rhs._value[rhs._value.size()-1-i]);
     }

     //Compute the FFT of each
     fft(lhs_fft);
     fft(rhs_fft);

     //Perform pointwise multiplication (numbers are complex)
     std::deque<double> out_fft(2*size);
     for (size_t i = 0; i < 2*size; i+=2){
          out_fft[i] = lhs_fft[i]*rhs_fft[i] - lhs_fft[i+1]*rhs_fft[i+1];
          out_fft[i+1] = lhs_fft[i]*rhs_fft[i+1] + lhs_fft[i+1]*rhs_fft[i];
     }

     //Compute the inverse FFT of this number
     //remember to properly scale afterwards!
     fft(out_fft, false);
     for (size_t i = 0; i < 2*size; i++){
          out_fft[i] /= size;
     }

     //Convert back to integer, carrying along the way
     double carry = 0;
     integer out;
     for (size_t i = 0; i < 2*size; i+=2){
          double current = out_fft[i]+carry;
          if (current > double(integer::NEG1)){
               carry = current / (double(integer::NEG1)+1);
               carry = double(floor(carry+0.0001));
               current = current - (carry*(integer::NEG1+1));

          }
          else {
               carry = 0;
          }
          out._value.push_front(INTEGER_DIGIT_T(current+0.0001));
     }

     //Finish up
     return out;
}

integer integer::operator*(const integer & rhs) const {
    // quick checks
    if (!*this || !rhs){    // if multiplying by 0
        return 0;
    }
    if (*this == 1){        // if multiplying by 1
        return rhs;
    }
    if (rhs == 1){          // if multiplying by 1
        return *this;
    }

    // integer out = peasant(*this, rhs);
    // integer out = recursive_peasant(*this, rhs);
    // integer out = recursive_mult(*this, rhs);
    // integer out = karatsuba(*this, rhs);
    // integer out = toom_cook_3(*this, rhs);
    // integer out = long_mult(*this, rhs);
    integer out = fft_mult(*this, rhs);
    out._sign = _sign ^ rhs._sign;
    out.trim();
    return out;
}

integer & integer::operator*=(const integer & rhs){
    return *this = *this * rhs;
}

// // Naive Division: keep subtracting until lhs == 0
// std::pair <integer, integer> integer::naive_divmod(const integer & lhs, const integer & rhs) const {
    // std::pair <integer, integer> qr (0, lhs);
    // while (qr.second >= rhs){
        // qr.second -= rhs;
        // qr.first++;
    // }
    // return qr;
// }

// // Long Division returning both quotient and remainder
// std::pair <integer, integer> integer::long_divmod(const integer & lhs, const integer & rhs) const {
   // std::pair <integer, integer> qr(0, lhs);
   // integer copyd = rhs;
   // integer adder = 1;
   // integer shift = qr.second.bits() - copyd.bits();
    // copyd <<= shift;
    // adder <<= shift;
   // // while (qr.second > copyd){
       // // copyd <<= 1;
       // // adder <<= 1;
   // // }
   // while (qr.second >= rhs){
       // if (qr.second >= copyd){
           // qr.second -= copyd;
           // qr.first |= adder;
       // }
       // copyd >>= 1;
       // adder >>= 1;
   // }
   // return qr;
// }

// // Recursive Division that returns both the quotient and remainder
// // Recursion took up way too much memory
// std::pair <integer, integer> integer::recursive_divmod(const integer & lhs, const integer & rhs) const {
   // std::pair <integer, integer> qr;
   // if (!lhs){
       // qr.first = 0;
       // qr.second = 0;
       // return qr;
   // }
   // qr = recursive_divmod(lhs >> 1, rhs);
   // qr.first <<= 1;
   // qr.second <<= 1;
   // if (lhs & 1)
       // qr.second++;
   // if (qr.second >= rhs){
       // qr.second -= rhs;
       // qr.first++;
   // }
   // return qr;
// }

// Non-Recursive version of above algorithm
std::pair <integer, integer> integer::non_recursive_divmod(const integer & lhs, const integer & rhs) const {
    std::pair <integer, integer> qr (0, 0);
    for(integer::REP_SIZE_T x = lhs.bits(); x > 0; x--){
        qr.first  <<= 1;
        qr.second <<= 1;

        if (lhs[x - 1]){
            qr.second++;
        }

        if (qr.second >= rhs){
            qr.second -= rhs;
            qr.first++;
        }
    }
    return qr;
}

// division and modulus ignoring signs
std::pair <integer, integer> integer::dm(const integer & lhs, const integer & rhs) const {
    if (!rhs){              // divide by 0 error
        throw std::domain_error("Error: division or modulus by 0");
    }

    if (rhs == 1){          // divide by 1 check
        return {lhs, 0};
    }
    if (lhs == rhs){        // divide by same value check
        return {1, 0};
    }
    if (!lhs){              // 0 / rhs check
        return {0, 0};
    }
    if (lt(lhs, rhs)){      // lhs < rhs check
        return {0, lhs};
    }

    // return naive_divmod(lhs, rhs);
    // return long_divmod(lhs, rhs);
    // return recursive_divmod(lhs, rhs);
    return non_recursive_divmod(lhs, rhs);
}

// division and modulus with signs
std::pair <integer, integer> integer::divmod(const integer & lhs, const integer & rhs) const {
    std::pair <integer, integer> out = dm(abs(lhs), abs(rhs));
    out.first._sign = lhs._sign ^ rhs._sign;

    if (lhs._sign == integer::NEGATIVE){
        out.second = -out.second;
    }

    // if      ((lhs._sign == integer::POSITIVE) &&    // + % +
             // (rhs._sign == integer::POSITIVE)){
        // // out.second = out.second;
    // }
    // else if ((lhs._sign == integer::POSITIVE) &&    // + % -
             // (rhs._sign == integer::NEGATIVE)){
        // // out.second = out.second;
    // }
    // else if ((lhs._sign == integer::NEGATIVE) &&    // - % +
             // (rhs._sign == integer::POSITIVE)){
        // out.second = -out.second;
    // }
    // else if ((lhs._sign == integer::NEGATIVE) &&    // - % -
             // (rhs._sign == integer::NEGATIVE)){
        // out.second = -out.second;
    // }

    return out;
}

integer integer::operator/(const integer & rhs) const {
    return divmod(*this, rhs).first;
}

integer & integer::operator/=(const integer & rhs){
    return *this = *this / integer(rhs);
}

integer integer::operator%(const integer & rhs) const {
    return divmod(*this, rhs).second;
}

integer & integer::operator%=(const integer & rhs){
    return *this = *this % rhs;
}

// Prefix ++
integer & integer::operator++(){
    return *this += 1;
}

// Postfix ++
integer integer::operator++(int){
    integer temp(*this);
    ++*this;
    return temp;
}

// Prefix --
integer & integer::operator--(){
    return *this -= 1;
}

// Postfix --
integer integer::operator--(int){
    integer temp(*this);
    --*this;
    return temp;
}

// Nothing done since promotion doesnt work here
integer integer::operator+() const {
    return *this;
}

// Flip Sign
integer integer::operator-() const {
    return integer(_value, !_sign);
}

// get private values
integer::Sign integer::sign() const {
    return _sign;
}

// get minimum number of bits needed to hold this value
integer integer::bits() const {
    integer         out = integer(_value.empty()?0:(_value.size() - 1)) * integer::BITS;
    INTEGER_DIGIT_T msb = _value.empty()?0:_value[0];
    while (msb){
        msb >>= 1;
        out++;
    }

    return out;
}

// get minimum number of bytes needed to hold this value
integer::REP_SIZE_T integer::bytes() const {
    integer::REP_SIZE_T out = (_value.empty()?0:(_value.size() - 1)) * integer::OCTETS;
    INTEGER_DIGIT_T     msb = (_value.empty()?0:_value[0]);
    while (msb){
        msb >>= 8;
        out++;
    }

    return out;
}

// get number of digits
integer::REP_SIZE_T integer::digits() const {
    return _value.size();
}

// get internal data
integer::REP integer::data() const {
    return _value;
}

// Miscellaneous Functions
integer & integer::negate(){
    _sign = !_sign;
    return trim();
}

integer integer::twos_complement(const integer::REP_SIZE_T & b) const {
    integer mask; mask.fill(b);
    integer out = ((abs(*this) ^ mask) + 1) & mask;
    out._sign = !_sign;
    return out.trim();
}

// fills an integer with 1s
integer & integer::fill(const integer::REP_SIZE_T & b){
    _value = integer::REP(b / integer::BITS, integer::NEG1);
    if (b % integer::BITS){
        _value.push_front((1 << (b % integer::BITS)) - 1);
    }
    return *this;
}

// get bit, where 0 is the lsb and bits() - 1 is the msb
bool integer::operator[](const integer::REP_SIZE_T & b) const {
    if (b >= bits()){ // if given index is larger than bits in this _value, return 0
        return 0;
    }
    return (_value[_value.size() - (b / integer::BITS) - 1] >> (b % integer::BITS)) & 1;
}

// Output value as a string from base 2 to 16, or base 256
std::string integer::str(const integer & base, const std::string::size_type & length) const {
    std::string out = "";
    if ((2 <= base) && (base <= 16)){
        static const std::string digits = "0123456789abcdef";
        integer rhs = abs(*this);       // use absolute value to make sure index stays small
        if (*this == 0){
            out = "0";
        }
        else{
            std::pair <integer, integer> qr;
            do{
                qr = dm(rhs, base);
                out = digits[qr.second] + out;
                rhs = qr.first;
            } while (rhs);
        }

        // pad with '0's
        if (out.size() < length){
            out = std::string(length - out.size(), '0') + out;
        }
    }
    else if (base == 256){
        if (_value.empty()){
            out = std::string(1, 0);
        }
        else{
            // for each digit
            for(INTEGER_DIGIT_T const & d : _value){
                // write out each character
                for(std::size_t i = integer::OCTETS << 3; i > 0; i -= 8){
                    out += std::string(1, (d >> (i - 8)) & 0xff);
                }
            }

            // remove leading '\x00's
            // this is possible because _value is being read
            // one character at a time, not one byte at a time
            if (out.size() > length){
                std::size_t i = 0;
                while ((i < out.size()) && !out[i]){
                    i++;
                }

                out = out.substr(i, out.size() - i);
            }
        }

        // pad with '\x00's
        if (out.size() < length){
            out = std::string(length - out.size(), '\x00') + out;
        }
    }
    else{
        throw std::runtime_error("Error: Cannot convert to base " + base.str(10));
    }

    // if value is negative, add a minus sign in front
    // no special case for leading zeros/nulls
    if (_sign == integer::NEGATIVE){
        out = "-" + out;
    }

    return out;
}

// Bitshift Operators
integer operator<<(const bool & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const uint8_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const uint16_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const uint32_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const uint64_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const int8_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const int16_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const int32_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator<<(const int64_t & lhs, const integer & rhs){
    return integer(lhs) << rhs;
}

integer operator>>(const bool & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const uint8_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const uint16_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const uint32_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const uint64_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const int8_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const int16_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const int32_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

integer operator>>(const int64_t & lhs, const integer & rhs){
    return integer(lhs) >> rhs;
}

// IO Operators
std::ostream & operator<<(std::ostream & stream, const integer & rhs){
    if (stream.flags() & stream.oct){
        stream << rhs.str(8);
    }
    else if (stream.flags() & stream.hex){
        stream << rhs.str(16);
    }
    else{
        stream << rhs.str(10);
    }
    return stream;
}

std::istream & operator>>(std::istream & stream, integer & rhs){
    uint8_t base;
    if (stream.flags() & stream.oct){
        base = 8;
    }
    else if (stream.flags() & stream.hex){
        base = 16;
    }
    else{
        base = 10;
    }
    std::string in;
    stream >> in;
    rhs = integer(in, base);
    return stream;
}

// Special functions
std::string makebin(const integer & value, const unsigned int & size){
    // Changes a value into its binary string
    return value.str(2, size);
}

std::string makehex(const integer & value, const unsigned int & size){
    // Changes a value into its hexadecimal string
    return value.str(16, size);
}

std::string makeascii(const integer & value, const unsigned int & size){
    // Changes a value into ASCII
    return value.str(256, size);
}

integer abs(const integer & value){
    return (value.sign() == integer::POSITIVE)?value:-value;
}

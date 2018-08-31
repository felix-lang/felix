#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Comparison, less_than_or_equals){
    const uint256_t big  (0xffffffffffffffffULL, 0xffffffffffffffffULL);
    const uint256_t small(0x0000000000000000ULL, 0x0000000000000000ULL);

    EXPECT_EQ(small <= small,  true);
    EXPECT_EQ(small <= big,    true);

    EXPECT_EQ(big <= small,   false);
    EXPECT_EQ(big <= big,      true);
}

#define unsigned_compare_lte(Z)                                         \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T small = std::numeric_limits <Z>::min();                     \
    const T big   = std::numeric_limits <Z>::max();                     \
                                                                        \
    const uint256_t int_small(small);                                   \
    const uint256_t int_big(big);                                       \
}                                                                       \
while (0)

#define signed_compare_lte(Z)                                           \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T small =  1;                                                 \
    const T big = std::numeric_limits <Z>::max();                       \
                                                                        \
    const uint256_t int_small(small);                                   \
    const uint256_t int_big(big);                                       \
}                                                                       \
while (0)

// TEST(External, less_than_or_equals){
    // unsigned_compare_lte(bool);
    // unsigned_compare_lte(uint8_t);
    // unsigned_compare_lte(uint16_t);
    // unsigned_compare_lte(uint32_t);
    // unsigned_compare_lte(uint64_t);
    // signed_compare_lte(int8_t);
    // signed_compare_lte(int16_t);
    // signed_compare_lte(int32_t);
    // signed_compare_lte(int64_t);
// }

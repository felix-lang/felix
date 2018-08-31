#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Comparison, less_than){
    const uint256_t big  (0xffffffffffffffffULL, 0xffffffffffffffffULL);
    const uint256_t small(0x0000000000000000ULL, 0x0000000000000000ULL);

    EXPECT_EQ(small < small, false);
    EXPECT_EQ(small < big,    true);

    EXPECT_EQ(big < small,   false);
    EXPECT_EQ(big < big,     false);
}

#define unsigned_compare_lt(Z)                                          \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T small = std::numeric_limits <Z>::min();                     \
    const T big   = std::numeric_limits <Z>::max();                     \
                                                                        \
    const uint256_t int_small(small);                                   \
    const uint256_t int_big(big);                                       \
                                                                        \
    EXPECT_EQ(small > int_small, false);                                \
    EXPECT_EQ(small > int_big,   false);                                \
                                                                        \
    EXPECT_EQ(big > int_small,    true);                                \
    EXPECT_EQ(big > int_big,     false);                                \
}                                                                       \
while (0)

#define signed_compare_lt(Z)                                            \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T small =  1;                                                 \
    const T big = std::numeric_limits <Z>::max();                       \
                                                                        \
    const uint256_t int_small(small);                                   \
    const uint256_t int_big(big);                                       \
                                                                        \
    EXPECT_EQ(small > int_small, false);                                \
    EXPECT_EQ(small > int_big,   false);                                \
                                                                        \
    EXPECT_EQ(big > int_small,    true);                                \
    EXPECT_EQ(big > int_big,     false);                                \
}                                                                       \
while (0)

// TEST(External, less_than){
    // unsigned_compare_lt(bool);
    // unsigned_compare_lt(uint8_t);
    // unsigned_compare_lt(uint16_t);
    // unsigned_compare_lt(uint32_t);
    // unsigned_compare_lt(uint64_t);
    // signed_compare_lt(int8_t);
    // signed_compare_lt(int16_t);
    // signed_compare_lt(int32_t);
    // signed_compare_lt(int64_t);
// }

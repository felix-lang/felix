#include <gtest/gtest.h>

#include "integer.h"

TEST(Comparison, less_than_or_equals){
    const integer neg_big("-ffffffffffffffffff", 16);
    const integer neg_small("-1",                16);
    const integer pos_small( "1",                16);
    const integer pos_big("ffffffffffffffffff",  16);

    EXPECT_EQ(neg_big <= neg_big,      true);
    EXPECT_EQ(neg_big <= neg_small,    true);
    EXPECT_EQ(neg_big <= pos_small,    true);
    EXPECT_EQ(neg_big <= pos_big,      true);

    EXPECT_EQ(neg_small <= neg_big,   false);
    EXPECT_EQ(neg_small <= neg_small,  true);
    EXPECT_EQ(neg_small <= pos_small,  true);
    EXPECT_EQ(neg_small <= pos_big,    true);

    EXPECT_EQ(pos_small <= neg_big,   false);
    EXPECT_EQ(pos_small <= neg_small, false);
    EXPECT_EQ(pos_small <= pos_small,  true);
    EXPECT_EQ(pos_small <= pos_big,    true);

    EXPECT_EQ(pos_big <= neg_big,     false);
    EXPECT_EQ(pos_big <= neg_small,   false);
    EXPECT_EQ(pos_big <= pos_small,   false);
    EXPECT_EQ(pos_big <= pos_big,      true);
}

#define unsigned_compare_lte(Z)                                         \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T pos_small = std::numeric_limits <Z>::min();                 \
    const T pos_big   = std::numeric_limits <Z>::max();                 \
                                                                        \
    const integer int_pos_small(pos_small);                             \
    const integer int_pos_big(pos_big);                                 \
                                                                        \
}                                                                       \
while (0)

#define signed_compare_lte(Z)                                           \
do                                                                      \
{                                                                       \
    static_assert(std::is_signed <Z>::value, "Type must be signed");    \
                                                                        \
    const T neg_big = std::numeric_limits <Z>::min();                   \
    const T neg_small = -1;                                             \
    const T pos_small =  1;                                             \
    const T pos_big = std::numeric_limits <Z>::max();                   \
                                                                        \
    const integer int_neg_big(neg_big);                                 \
    const integer int_neg_small(neg_small);                             \
    const integer int_pos_small(pos_small);                             \
    const integer int_pos_big(pos_big);                                 \
                                                                        \
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

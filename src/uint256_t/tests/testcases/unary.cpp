#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Arithmetic, unary_plus){
    const uint256_t value(0x12345ULL);
    EXPECT_EQ(+value, value);
}

TEST(Arithmetic, unary_minus){
    const uint256_t val(1);
    const uint256_t neg = -val;
    EXPECT_EQ(-val, neg);
    EXPECT_EQ(-neg, val);
    EXPECT_EQ(neg, uint256_t(0xffffffffffffffffULL, 0xffffffffffffffffULL, 0xffffffffffffffffULL, 0xffffffffffffffffULL));
}
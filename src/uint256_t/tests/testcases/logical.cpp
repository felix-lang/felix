#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Logical, and){
    const uint256_t A(0xffffffff);
    const uint256_t B(0x00000000);

    EXPECT_EQ(A && A, true);
    EXPECT_EQ(A && B, false);
}

TEST(Logical, or){
    const uint256_t A(0xffffffff);
    const uint256_t B(0x00000000);

    EXPECT_EQ(A || A, true);
    EXPECT_EQ(A || B, true);
}

TEST(Logical, not){
    EXPECT_EQ(!uint256_t(0xffffffff), 0);
}
#include <gtest/gtest.h>

#include "integer.h"

TEST(Logical, and){
    const integer A("ffffffff", 16);
    const integer B("00000000", 16);

    EXPECT_EQ(A && A, true);
    EXPECT_EQ(A && B, false);
}

TEST(Logical, or){
    const integer A("ffffffff", 16);
    const integer B("00000000", 16);

    EXPECT_EQ(A || A, true);
    EXPECT_EQ(A || B, true);
}

TEST(Logical, not){
    EXPECT_EQ(!integer("ffffffff", 16), 0);
}
#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, unary_plus){
    const integer value("12345", 16);
    EXPECT_EQ(+value, value);
}

TEST(Arithmetic, unary_minus){
    const integer pos("12345", 16);
    const integer neg = -pos;
    EXPECT_EQ(-pos, neg);
    EXPECT_EQ(-neg, pos);
}
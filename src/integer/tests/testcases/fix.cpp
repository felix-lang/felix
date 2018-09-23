#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, increment){
    integer value(0);
    EXPECT_EQ(++value, 1);
    EXPECT_EQ(value++, 1);
    EXPECT_EQ(++value, 3);
}

TEST(Arithmetic, decrement){
    integer value(0);
    EXPECT_EQ(--value, -1);
    EXPECT_EQ(value--, -1);
    EXPECT_EQ(--value, -3);
}
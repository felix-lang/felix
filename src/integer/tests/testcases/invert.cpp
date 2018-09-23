#include <gtest/gtest.h>

#include "integer.h"

TEST(BitWise, invert){
    EXPECT_EQ(~integer("00000000", 16), 1);
    EXPECT_EQ(~integer("deadbeef", 16), 0x21524110);
    EXPECT_EQ(~integer("ffffffff", 16), 0);
}
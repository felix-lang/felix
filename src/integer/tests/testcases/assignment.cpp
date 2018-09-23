#include <gtest/gtest.h>

#include "integer.h"

TEST(Assignment, all){
    EXPECT_EQ(integer((bool)     true).str(16),               "1");
    EXPECT_EQ(integer((bool)     false).str(16),              "0");
    EXPECT_EQ(integer((uint8_t)  0xef).str(16),               "ef");
    EXPECT_EQ(integer((uint16_t) 0xbeef).str(16),             "beef");
    EXPECT_EQ(integer((uint32_t) 0xdeadbeef).str(16),         "deadbeef");
    EXPECT_EQ(integer((uint64_t) 0xfee1baaddeadbeef).str(16), "fee1baaddeadbeef");
    EXPECT_EQ(integer((int8_t)   0xef).str(16),               "-11");
    EXPECT_EQ(integer((int16_t)  0xbeef).str(16),             "-4111");
    EXPECT_EQ(integer((int32_t)  0xdeadbeef).str(16),         "-21524111");
    EXPECT_EQ(integer((int64_t)  0xfee1baaddeadbeef).str(16), "-11e455221524111");
}
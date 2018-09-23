#include <gtest/gtest.h>

#include "integer.h"

TEST(Comparison, equals){
    EXPECT_EQ( (integer("deadbeef", 16) == integer("deadbeef", 16)), true);
    EXPECT_EQ(!(integer("deadbeef", 16) == integer("fee1baad", 16)), true);
}

TEST(External, equals){
    const bool     t   = true;
    const bool     f   = false;
    const uint8_t  u8  = 0xaa;
    const uint16_t u16 = 0xaaaa;
    const uint32_t u32 = 0xaaaaaaaaUL;
    const uint64_t u64 = 0xaaaaaaaaaaaaaaaaULL;
    const int8_t   i8  = 0xaa;
    const int16_t  i16 = 0xaaaa;
    const int32_t  i32 = 0xaaaaaaaaL;
    const int64_t  i64 = 0xaaaaaaaaaaaaaaaaLL;

    EXPECT_EQ(t,   integer(t));
    EXPECT_EQ(f,   integer(f));
    EXPECT_EQ(u8,  integer(u8));
    EXPECT_EQ(u16, integer(u16));
    EXPECT_EQ(u32, integer(u32));
    EXPECT_EQ(u64, integer(u64));
    EXPECT_EQ(i8,  integer(i8));
    EXPECT_EQ(i16, integer(i16));
    EXPECT_EQ(i32, integer(i32));
    EXPECT_EQ(i64, integer(i64));
}

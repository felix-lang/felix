#include <gtest/gtest.h>

#include "integer.h"

TEST(Comparison, not_equals){
    EXPECT_EQ(!(integer("deadbeef", 16) != integer("deadbeef", 16)), true);
    EXPECT_EQ( (integer("deadbeef", 16) != integer("fee1baad", 16)), true);
}

TEST(External, not_equals){
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

    EXPECT_EQ((t   != integer(f)),   true);
    EXPECT_EQ((f   != integer(t)),   true);
    EXPECT_EQ((u8  != integer(i64)), true);
    EXPECT_EQ((u16 != integer(i32)), true);
    EXPECT_EQ((u32 != integer(i16)), true);
    EXPECT_EQ((u64 != integer(i8)),  true);
    EXPECT_EQ((i8  != integer(u64)), true);
    EXPECT_EQ((i16 != integer(u32)), true);
    EXPECT_EQ((i32 != integer(u16)), true);
    EXPECT_EQ((i64 != integer(u8)),  true);

    EXPECT_EQ((t   != integer(t)),   false);
    EXPECT_EQ((f   != integer(f)),   false);
    EXPECT_EQ((u8  != integer(u8)),  false);
    EXPECT_EQ((u16 != integer(u16)), false);
    EXPECT_EQ((u32 != integer(u32)), false);
    EXPECT_EQ((u64 != integer(u64)), false);
    EXPECT_EQ((i8  != integer(i8)),  false);
    EXPECT_EQ((i16 != integer(i16)), false);
    EXPECT_EQ((i32 != integer(i32)), false);
    EXPECT_EQ((i64 != integer(i64)), false);
}
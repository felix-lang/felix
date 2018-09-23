#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, add){
    integer pos( "fedbca9876543210", 16);
    integer neg("-fedbca9876543210", 16);

    EXPECT_EQ((pos + pos).str(16),   "1fdb79530eca86420");
    EXPECT_EQ((pos + neg).str(16),   "0");
    EXPECT_EQ((neg + neg).str(16),  "-1fdb79530eca86420");

    EXPECT_EQ((pos += neg).str(16),  "0");
    EXPECT_EQ((neg += pos).str(16), "-fedbca9876543210");
}

TEST(External, add){
    bool     t   = true;
    bool     f   = false;
    uint8_t  u8  = 0xaa;
    uint16_t u16 = 0xaaaa;
    uint32_t u32 = 0xaaaaaaaaUL;
    uint64_t u64 = 0xaaaaaaaaaaaaaaaaULL;
    int8_t   i8  = 0xaa;
    int16_t  i16 = 0xaaaa;
    int32_t  i32 = 0xaaaaaaaaL;
    int64_t  i64 = 0xaaaaaaaaaaaaaaaaLL;

    const integer pos("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16);

    EXPECT_EQ(t   +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(f   +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16));
    EXPECT_EQ(u8  +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f19a", 16));
    EXPECT_EQ(u16 +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f19b9a", 16));
    EXPECT_EQ(u32 +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f19b9b9b9a", 16));
    EXPECT_EQ(u64 +  pos, integer("f0f0f0f0f0f0f0f19b9b9b9b9b9b9b9a", 16));
    EXPECT_EQ(i8  +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f09a", 16));
    EXPECT_EQ(i16 +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f09b9a", 16));
    EXPECT_EQ(i32 +  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f09b9b9b9a", 16));
    EXPECT_EQ(i64 +  pos, integer("f0f0f0f0f0f0f0f09b9b9b9b9b9b9b9a", 16));

    EXPECT_EQ(t   += pos, true);
    EXPECT_EQ(f   += pos, true);
    EXPECT_EQ(u8  += pos, (uint8_t)  0x9a);
    EXPECT_EQ(u16 += pos, (uint16_t) 0x9b9a);
    EXPECT_EQ(u32 += pos, (uint32_t) 0x9b9b9b9a);
    EXPECT_EQ(u64 += pos, (uint64_t) 0x9b9b9b9b9b9b9b9a);
    EXPECT_EQ(i8  += pos, (int8_t)   0x9a);
    EXPECT_EQ(i16 += pos, (int16_t)  0x9b9a);
    EXPECT_EQ(i32 += pos, (int32_t)  0x9b9b9b9a);
    EXPECT_EQ(i64 += pos, (int64_t)  0x9b9b9b9b9b9b9b9a);

    const integer neg = -pos;

    EXPECT_EQ(t   +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0ef", 16));
    EXPECT_EQ(f   +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0ef", 16));
    EXPECT_EQ(u8  +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f056", 16));
    EXPECT_EQ(u16 +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f05556", 16));
    EXPECT_EQ(u32 +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f055555556", 16));
    EXPECT_EQ(u64 +  neg, integer("-f0f0f0f0f0f0f0f05555555555555556", 16));
    EXPECT_EQ(i8  +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f156", 16));
    EXPECT_EQ(i16 +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f15556", 16));
    EXPECT_EQ(i32 +  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f155555556", 16));
    EXPECT_EQ(i64 +  neg, integer("-f0f0f0f0f0f0f0f15555555555555556", 16));

    EXPECT_EQ(t   += neg, true);
    EXPECT_EQ(f   += neg, true);
    EXPECT_EQ(u8  += neg, (uint8_t)  0xaa);
    EXPECT_EQ(u16 += neg, (uint16_t) 0xaaaa);
    EXPECT_EQ(u32 += neg, (uint32_t) 0xaaaaaaaa);
    EXPECT_EQ(u64 += neg, (uint64_t) 0xaaaaaaaaaaaaaaaa);
    EXPECT_EQ(i8  += neg, (int8_t)   0xaa);
    EXPECT_EQ(i16 += neg, (int16_t)  0xaaaa);
    EXPECT_EQ(i32 += neg, (int32_t)  0xaaaaaaaaaa);
    EXPECT_EQ(i64 += neg, (int64_t)  0xaaaaaaaaaaaaaaaa);
}

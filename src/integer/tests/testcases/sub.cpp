#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, subtract){
    integer pos ( "fedbca9876543210", 16);
    integer neg ("-fedbca9876543210", 16);

    EXPECT_EQ((pos -  pos).str(16), "0");
    EXPECT_EQ((pos -  neg).str(16), "1fdb79530eca86420");
    EXPECT_EQ((neg -  neg).str(16), "0");
    EXPECT_EQ((pos -= neg).str(16), "1fdb79530eca86420");
    EXPECT_EQ((neg -= pos).str(16), "-2fc935fc962fc9630");
}

TEST(External, subtract){
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

    EXPECT_EQ(t   -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0ef", 16));
    EXPECT_EQ(f   -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16));
    EXPECT_EQ(u8  -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f046", 16));
    EXPECT_EQ(u16 -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f04646", 16));
    EXPECT_EQ(u32 -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f046464646", 16));
    EXPECT_EQ(u64 -  pos, integer("-f0f0f0f0f0f0f0f04646464646464646", 16));
    EXPECT_EQ(i8  -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f146", 16));
    EXPECT_EQ(i16 -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f14646", 16));
    EXPECT_EQ(i32 -  pos, integer("-f0f0f0f0f0f0f0f0f0f0f0f146464646", 16));
    EXPECT_EQ(i64 -  pos, integer("-f0f0f0f0f0f0f0f14646464646464646", 16));

    EXPECT_EQ(t   -= pos, true);
    EXPECT_EQ(f   -= pos, true);
    EXPECT_EQ(u8  -= pos, (uint8_t)  -0x46);
    EXPECT_EQ(u16 -= pos, (uint16_t) -0x4646);
    EXPECT_EQ(u32 -= pos, (uint32_t) -0x46464646);
    EXPECT_EQ(u64 -= pos, (uint64_t) -0x4646464646464646);
    EXPECT_EQ(i8  -= pos, (int8_t)   -0x46);
    EXPECT_EQ(i16 -= pos, (int16_t)  -0x4646);
    EXPECT_EQ(i32 -= pos, (int32_t)  -0x46464646);
    EXPECT_EQ(i64 -= pos, (int64_t)  -0x4646464646464646);

    const integer neg = -pos;

    EXPECT_EQ(t   -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(f   -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(u8  -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1aa", 16));
    EXPECT_EQ(u16 -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f1aaaa", 16));
    EXPECT_EQ(u32 -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f1aaaaaaaa", 16));
    EXPECT_EQ(u64 -  neg, integer("f0f0f0f0f0f0f0f1aaaaaaaaaaaaaaaa", 16));
    EXPECT_EQ(i8  -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0aa", 16));
    EXPECT_EQ(i16 -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0aaaa", 16));
    EXPECT_EQ(i32 -  neg, integer("f0f0f0f0f0f0f0f0f0f0f0f0aaaaaaaa", 16));
    EXPECT_EQ(i64 -  neg, integer("f0f0f0f0f0f0f0f0aaaaaaaaaaaaaaaa", 16));

    EXPECT_EQ(t   -= neg, true);
    EXPECT_EQ(f   -= neg, true);
    EXPECT_EQ(u8  -= neg, (uint8_t)  0xaa);
    EXPECT_EQ(u16 -= neg, (uint16_t) 0xaaaa);
    EXPECT_EQ(u32 -= neg, (uint32_t) 0xaaaaaaaa);
    EXPECT_EQ(u64 -= neg, (uint64_t) 0xaaaaaaaaaaaaaaaa);
    EXPECT_EQ(i8  -= neg, (int8_t)   0xaa);
    EXPECT_EQ(i16 -= neg, (int16_t)  0xaaaa);
    EXPECT_EQ(i32 -= neg, (int32_t)  0xaaaaaaaaaa);
    EXPECT_EQ(i64 -= neg, (int64_t)  0xaaaaaaaaaaaaaaaa);
}
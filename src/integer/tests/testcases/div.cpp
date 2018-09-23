#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, divide){
    integer big_pos   = integer("fedbca9876543210", 16);
    integer small_pos = integer("ffff",             16);
    integer res_pos   = integer("fedcc9753fc9",     16);
    integer big_neg   = -big_pos;
    integer small_neg = -small_pos;
    integer res_neg   = -res_pos;

    EXPECT_EQ(big_neg   / big_neg,         1);
    EXPECT_EQ(big_neg   / small_neg, res_pos);
    EXPECT_EQ(big_neg   / small_pos, res_neg);
    EXPECT_EQ(big_neg   / big_pos,        -1);

    EXPECT_EQ(small_neg / big_neg,         0);
    EXPECT_EQ(small_neg / small_neg,       1);
    EXPECT_EQ(small_neg / small_pos,      -1);
    EXPECT_EQ(small_neg / big_pos,         0);

    EXPECT_EQ(small_pos / big_neg,         0);
    EXPECT_EQ(small_pos / small_neg,      -1);
    EXPECT_EQ(small_pos / small_pos,       1);
    EXPECT_EQ(small_pos / big_pos,         0);

    EXPECT_EQ(big_pos   / big_neg,        -1);
    EXPECT_EQ(big_pos   / small_neg, res_neg);
    EXPECT_EQ(big_pos   / small_pos, res_pos);
    EXPECT_EQ(big_pos   / big_pos,         1);

    EXPECT_THROW(integer(1) / integer(0), std::domain_error);
}

TEST(External, divide){
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

    const integer pos("7b", 16);

    EXPECT_EQ(t   /  pos, false);
    EXPECT_EQ(f   /  pos, false);
    EXPECT_EQ(u8  /  pos, integer( "1",               16));
    EXPECT_EQ(u16 /  pos, integer( "163",             16));
    EXPECT_EQ(u32 /  pos, integer( "163356b",         16));
    EXPECT_EQ(u64 /  pos, integer( "163356b88ac0de0", 16));
    EXPECT_EQ(i8  /  pos, integer( "0",               16));
    EXPECT_EQ(i16 /  pos, integer("-b1",              16));
    EXPECT_EQ(i32 /  pos, integer("-b19ab5",          16));
    EXPECT_EQ(i64 /  pos, integer("-b19ab5c45606f0",  16));

    EXPECT_EQ(t   /= pos, false);
    EXPECT_EQ(f   /= pos, false);
    EXPECT_EQ(u8  /= pos, (uint8_t)   0x1);
    EXPECT_EQ(u16 /= pos, (uint16_t)  0x163);
    EXPECT_EQ(u32 /= pos, (uint32_t)  0x163356b);
    EXPECT_EQ(u64 /= pos, (uint64_t)  0x163356b88ac0de0);
    EXPECT_EQ(i8  /= pos, (int8_t)    0);
    EXPECT_EQ(i16 /= pos, (int16_t)  -0xb1);
    EXPECT_EQ(i32 /= pos, (int32_t)  -0xb19ab5);
    EXPECT_EQ(i64 /= pos, (int64_t)  -0xb19ab5c45606f0);

    const integer neg = -pos;

    EXPECT_EQ(t   /  neg, 0);
    EXPECT_EQ(f   /  neg, 0);
    EXPECT_EQ(u8  /  neg, integer("-0",             16));
    EXPECT_EQ(u16 /  neg, integer("-2",             16));
    EXPECT_EQ(u32 /  neg, integer("-2e34b",         16));
    EXPECT_EQ(u64 /  neg, integer("-2e34bcd142169", 16));
    EXPECT_EQ(i8  /  neg, integer( "0",             16));
    EXPECT_EQ(i16 /  neg, integer( "1",             16));
    EXPECT_EQ(i32 /  neg, integer( "171a5",         16));
    EXPECT_EQ(i64 /  neg, integer( "171a5e68a10b4", 16));

    EXPECT_EQ(t   /= neg, false);
    EXPECT_EQ(f   /= neg, false);
    EXPECT_EQ(u8  /= neg, (uint8_t)  -0x0);
    EXPECT_EQ(u16 /= neg, (uint16_t) -0x2);
    EXPECT_EQ(u32 /= neg, (uint32_t) -0x2e34b);
    EXPECT_EQ(u64 /= neg, (uint64_t) -0x2e34bcd142169);
    EXPECT_EQ(i8  /= neg, (int8_t)    0x0);
    EXPECT_EQ(i16 /= neg, (int16_t)   0x01);
    EXPECT_EQ(i32 /= neg, (int32_t)   0x171a5);
    EXPECT_EQ(i64 /= neg, (int64_t)   0x171a5e68a10b4);
}

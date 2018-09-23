#include <gtest/gtest.h>

#include "integer.h"

TEST(BitWise, and){
    integer t  ((bool)     true);
    integer f  ((bool)     false);
    integer u8 ((uint8_t)  0xaa);
    integer u16((uint16_t) 0xaaaa);
    integer u32((uint32_t) 0xaaaaaaaa);
    integer u64((uint64_t) 0xaaaaaaaaaaaaaaaa);
    integer i8 ((int8_t)   0xaa);
    integer i16((int16_t)  0xaaaa);
    integer i32((int32_t)  0xaaaaaaaa);
    integer i64((int64_t)  0xaaaaaaaaaaaaaaaa);

    const integer pos("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16);

    // EXPECT_EQ(t   &  pos, integer("0",                                16));
    // EXPECT_EQ(f   &  pos, integer("0",                                16));
    // EXPECT_EQ(u8  &  pos, integer("a0",                               16));
    // EXPECT_EQ(u16 &  pos, integer("a0a0",                             16));
    // EXPECT_EQ(u32 &  pos, integer("a0a0a0a0",                         16));
    // EXPECT_EQ(u64 &  pos, integer("a0a0a0a0a0a0a0a0",                 16));
    // EXPECT_EQ(i8  &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0", 16));
    // EXPECT_EQ(i16 &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0a0", 16));
    // EXPECT_EQ(i32 &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0a0a0a0a0", 16));
    // EXPECT_EQ(i64 &  pos, integer("f0f0f0f0f0f0f0f0a0a0a0a0a0a0a0a0", 16));

    // EXPECT_EQ(t   &= pos, integer("0",                                16));
    // EXPECT_EQ(f   &= pos, integer("0",                                16));
    // EXPECT_EQ(u8  &= pos, integer("a0",                               16));
    // EXPECT_EQ(u16 &= pos, integer("a0a0",                             16));
    // EXPECT_EQ(u32 &= pos, integer("a0a0a0a0",                         16));
    // EXPECT_EQ(u64 &= pos, integer("a0a0a0a0a0a0a0a0",                 16));
    // EXPECT_EQ(i8  &= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0", 16));
    // EXPECT_EQ(i16 &= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0a0", 16));
    // EXPECT_EQ(i32 &= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0a0a0a0a0", 16));
    // EXPECT_EQ(i64 &= pos, integer("f0f0f0f0f0f0f0f0a0a0a0a0a0a0a0a0", 16));

    // const integer neg = -pos;

    // // A & X & ~X = 0
    // EXPECT_EQ(t   &  neg, 0);
    // EXPECT_EQ(f   &  neg, 0);
    // EXPECT_EQ(u8  &  neg, 0);
    // EXPECT_EQ(u16 &  neg, 0);
    // EXPECT_EQ(u32 &  neg, 0);
    // EXPECT_EQ(u64 &  neg, 0);
    // EXPECT_EQ(i8  &  neg, 0);
    // EXPECT_EQ(i16 &  neg, 0);
    // EXPECT_EQ(i32 &  neg, 0);
    // EXPECT_EQ(i64 &  neg, 0);

    // EXPECT_EQ(t   &= neg, 0);
    // EXPECT_EQ(f   &= neg, 0);
    // EXPECT_EQ(u8  &= neg, 0);
    // EXPECT_EQ(u16 &= neg, 0);
    // EXPECT_EQ(u32 &= neg, 0);
    // EXPECT_EQ(u64 &= neg, 0);
    // EXPECT_EQ(i8  &= neg, 0);
    // EXPECT_EQ(i16 &= neg, 0);
    // EXPECT_EQ(i32 &= neg, 0);
    // EXPECT_EQ(i64 &= neg, 0);

    // // zero
    // EXPECT_EQ(integer() & pos, 0);
    // EXPECT_EQ(integer() & neg, 0);
}

TEST(External, and){
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

    EXPECT_EQ(t   &  pos, integer("0",                                16));
    EXPECT_EQ(f   &  pos, integer("0",                                16));
    EXPECT_EQ(u8  &  pos, integer("a0",                               16));
    EXPECT_EQ(u16 &  pos, integer("a0a0",                             16));
    EXPECT_EQ(u32 &  pos, integer("a0a0a0a0",                         16));
    EXPECT_EQ(u64 &  pos, integer("a0a0a0a0a0a0a0a0",                 16));
    EXPECT_EQ(i8  &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0", 16));
    EXPECT_EQ(i16 &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0a0", 16));
    EXPECT_EQ(i32 &  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0a0a0a0a0", 16));
    EXPECT_EQ(i64 &  pos, integer("f0f0f0f0f0f0f0f0a0a0a0a0a0a0a0a0", 16));

    EXPECT_EQ(t   &= pos, false);
    EXPECT_EQ(f   &= pos, false);
    EXPECT_EQ(u8  &= pos, (uint8_t)  0xa0);
    EXPECT_EQ(u16 &= pos, (uint16_t) 0xa0a0);
    EXPECT_EQ(u32 &= pos, (uint32_t) 0xa0a0a0a0);
    EXPECT_EQ(u64 &= pos, (uint64_t) 0xa0a0a0a0a0a0a0a0);
    EXPECT_EQ(i8  &= pos, (int8_t)   0xa0);
    EXPECT_EQ(i16 &= pos, (int16_t)  0xa0a0);
    EXPECT_EQ(i32 &= pos, (int32_t)  0xa0a0a0a0);
    EXPECT_EQ(i64 &= pos, (int64_t)  0xa0a0a0a0a0a0a0a0);

    const integer neg = -pos;

    EXPECT_EQ(t   &  neg, false);
    EXPECT_EQ(f   &  neg, false);
    EXPECT_EQ(u8  &  neg, 0);
    EXPECT_EQ(u16 &  neg, 0);
    EXPECT_EQ(u32 &  neg, 0);
    EXPECT_EQ(u64 &  neg, 0);
    EXPECT_EQ(i8  &  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f100", 16));
    EXPECT_EQ(i16 &  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f10000", 16));
    EXPECT_EQ(i32 &  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f100000000", 16));
    EXPECT_EQ(i64 &  neg, integer("-f0f0f0f0f0f0f0f10000000000000000", 16));

    EXPECT_EQ(t   &= neg, false);
    EXPECT_EQ(f   &= neg, false);
    EXPECT_EQ(u8  &= neg, (uint8_t)  0);
    EXPECT_EQ(u16 &= neg, (uint16_t) 0);
    EXPECT_EQ(u32 &= neg, (uint32_t) 0);
    EXPECT_EQ(u64 &= neg, (uint64_t) 0);
    EXPECT_EQ(i8  &= neg, (int8_t)   0);
    EXPECT_EQ(i16 &= neg, (int16_t)  0);
    EXPECT_EQ(i32 &= neg, (int32_t)  0);
    EXPECT_EQ(i64 &= neg, (int64_t)  0);

    // zero
    EXPECT_EQ(integer() & pos, 0);
    EXPECT_EQ(integer() & neg, 0);
}

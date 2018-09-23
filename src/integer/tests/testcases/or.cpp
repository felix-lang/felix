#include <gtest/gtest.h>

#include "integer.h"

TEST(BitWise, or){
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

    EXPECT_EQ(t   |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(f   |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16));
    EXPECT_EQ(u8  |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0fa", 16));
    EXPECT_EQ(u16 |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0fafa", 16));
    EXPECT_EQ(u32 |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0fafafafa", 16));
    EXPECT_EQ(u64 |  pos, integer("f0f0f0f0f0f0f0f0fafafafafafafafa", 16));
    EXPECT_EQ(i8  |  pos, integer("-6",                               16));
    EXPECT_EQ(i16 |  pos, integer("-506",                             16));
    EXPECT_EQ(i32 |  pos, integer("-5050506",                         16));
    EXPECT_EQ(i64 |  pos, integer("-505050505050506",                 16));

    EXPECT_EQ(t   |= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(f   |= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16));
    EXPECT_EQ(u8  |= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0fa", 16));
    EXPECT_EQ(u16 |= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0fafa", 16));
    EXPECT_EQ(u32 |= pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0fafafafa", 16));
    EXPECT_EQ(u64 |= pos, integer("f0f0f0f0f0f0f0f0fafafafafafafafa", 16));
    EXPECT_EQ(i8  |= pos, integer("-6",                               16));
    EXPECT_EQ(i16 |= pos, integer("-506",                             16));
    EXPECT_EQ(i32 |= pos, integer("-5050506",                         16));
    EXPECT_EQ(i64 |= pos, integer("-505050505050506",                 16));

    const integer neg = -pos;

    EXPECT_EQ(t   |  neg, integer("-f",  16));
    EXPECT_EQ(f   |  neg, integer("-10", 16));
    EXPECT_EQ(u8  |  neg, integer("-6",  16));
    EXPECT_EQ(u16 |  neg, integer("-6",  16));
    EXPECT_EQ(u32 |  neg, integer("-6",  16));
    EXPECT_EQ(u64 |  neg, integer("-6",  16));
    EXPECT_EQ(i8  |  neg, integer("-6",  16));
    EXPECT_EQ(i16 |  neg, integer("-6",  16));
    EXPECT_EQ(i32 |  neg, integer("-6",  16));
    EXPECT_EQ(i64 |  neg, integer("-6",  16));

    EXPECT_EQ(t   |= neg, integer("-f",  16));
    EXPECT_EQ(f   |= neg, integer("-10", 16));
    EXPECT_EQ(u8  |= neg, integer("-6",  16));
    EXPECT_EQ(u16 |= neg, integer("-6",  16));
    EXPECT_EQ(u32 |= neg, integer("-6",  16));
    EXPECT_EQ(u64 |= neg, integer("-6",  16));
    EXPECT_EQ(i8  |= neg, integer("-6",  16));
    EXPECT_EQ(i16 |= neg, integer("-6",  16));
    EXPECT_EQ(i32 |= neg, integer("-6",  16));
    EXPECT_EQ(i64 |= neg, integer("-6",  16));

    // zero
    EXPECT_EQ(integer() | pos, pos);
    EXPECT_EQ(integer() | neg, neg);
}

TEST(External, or){
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

    EXPECT_EQ(t   |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f1", 16));
    EXPECT_EQ(f   |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0", 16));
    EXPECT_EQ(u8  |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0fa", 16));
    EXPECT_EQ(u16 |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0f0f0fafa", 16));
    EXPECT_EQ(u32 |  pos, integer("f0f0f0f0f0f0f0f0f0f0f0f0fafafafa", 16));
    EXPECT_EQ(u64 |  pos, integer("f0f0f0f0f0f0f0f0fafafafafafafafa", 16));
    EXPECT_EQ(i8  |  pos, integer("-6",                               16));
    EXPECT_EQ(i16 |  pos, integer("-506",                             16));
    EXPECT_EQ(i32 |  pos, integer("-5050506",                         16));
    EXPECT_EQ(i64 |  pos, integer("-505050505050506",                 16));

    EXPECT_EQ(t   |= pos, true);
    EXPECT_EQ(f   |= pos, true);
    EXPECT_EQ(u8  |= pos, (uint8_t)  0xfa);
    EXPECT_EQ(u16 |= pos, (uint16_t) 0xfafa);
    EXPECT_EQ(u32 |= pos, (uint32_t) 0xfafafafa);
    EXPECT_EQ(u64 |= pos, (uint64_t) 0xfafafafafafafafa);
    EXPECT_EQ(i8  |= pos, (int8_t)   0xfa);
    EXPECT_EQ(i16 |= pos, (int16_t)  0xfafa);
    EXPECT_EQ(i32 |= pos, (int32_t)  0xfafafafa);
    EXPECT_EQ(i64 |= pos, (int64_t)  0xfafafafafafafafa);

    const integer neg = -pos;

    EXPECT_EQ(t   |  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0ef", 16));
    EXPECT_EQ(f   |  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0ef", 16));
    EXPECT_EQ(u8  |  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f0f006", 16));
    EXPECT_EQ(u16 |  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f0f0f00006", 16));
    EXPECT_EQ(u32 |  neg, integer("-f0f0f0f0f0f0f0f0f0f0f0f000000006", 16));
    EXPECT_EQ(u64 |  neg, integer("-f0f0f0f0f0f0f0f00000000000000006", 16));
    EXPECT_EQ(i8  |  neg, integer("-6",  16));
    EXPECT_EQ(i16 |  neg, integer("-6",  16));
    EXPECT_EQ(i32 |  neg, integer("-6",  16));
    EXPECT_EQ(i64 |  neg, integer("-6",  16));

    EXPECT_EQ(t   |= neg, true);
    EXPECT_EQ(f   |= neg, true);
    EXPECT_EQ(u8  |= neg, (uint8_t)  -6);
    EXPECT_EQ(u16 |= neg, (uint16_t) -6);
    EXPECT_EQ(u32 |= neg, (uint32_t) -6);
    EXPECT_EQ(u64 |= neg, (uint64_t) -6);
    EXPECT_EQ(i8  |= neg, (int8_t)   -6);
    EXPECT_EQ(i16 |= neg, (int16_t)  -6);
    EXPECT_EQ(i32 |= neg, (int32_t)  -6);
    EXPECT_EQ(i64 |= neg, (int64_t)  -6);

    // zero
    EXPECT_EQ(integer() | pos, pos);
    EXPECT_EQ(integer() | neg, neg);
}

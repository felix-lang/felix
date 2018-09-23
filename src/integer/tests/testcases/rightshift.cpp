#include <gtest/gtest.h>

#include "integer.h"

TEST(BitShift, right){
    // operator>>
    integer pos("ffffffffffffffff", 16);
    uint64_t exp = 0xffffffffffffffffULL;
    for(uint8_t i = 0; i < 64; i++){
        EXPECT_EQ(pos >> i, exp >> i);
    }

    integer zero("0", 16);
    for(uint8_t i = 0; i < 64; i++){
        EXPECT_EQ(zero >> i, 0);
    }

    // operator>>=
    for(uint8_t i = 0; i < 64; i++){
        EXPECT_EQ(pos >>= 1, exp >>= 1);
    }

    for(uint8_t i = 0; i < 64; i++){
        EXPECT_EQ(zero >>= 1, 0);
    }

    // negatives don't match up because C++ does a literal bit shift
    // which results in lost bits, instead of dividing by 2
}

TEST(External, shift_right){
    bool     t   = true;
    bool     f   = false;
    uint8_t  u8  = 0xff;
    uint16_t u16 = 0xffff;
    uint32_t u32 = 0xffffffff;
    uint64_t u64 = 0xffffffffffffffff;
    int8_t   i8  = 0xff;
    int16_t  i16 = 0xffff;
    int32_t  i32 = 0xffffffff;
    int64_t  i64 = 0xffffffffffffffff;

    const integer zero(0);
    const integer one(1);

    EXPECT_EQ(t   >> zero, t);
    EXPECT_EQ(f   >> zero, f);
    EXPECT_EQ(u8  >> zero, u8);
    EXPECT_EQ(u16 >> zero, u16);
    EXPECT_EQ(u32 >> zero, u32);
    EXPECT_EQ(u64 >> zero, u64);
    EXPECT_EQ(i8  >> zero, i8);
    EXPECT_EQ(i16 >> zero, i16);
    EXPECT_EQ(i32 >> zero, i32);
    EXPECT_EQ(i64 >> zero, i64);

    EXPECT_EQ(t   >>= zero, t);
    EXPECT_EQ(f   >>= zero, f);
    EXPECT_EQ(u8  >>= zero, u8);
    EXPECT_EQ(u16 >>= zero, u16);
    EXPECT_EQ(u32 >>= zero, u32);
    EXPECT_EQ(u64 >>= zero, u64);
    EXPECT_EQ(i8  >>= zero, i8);
    EXPECT_EQ(i16 >>= zero, i16);
    EXPECT_EQ(i32 >>= zero, i32);
    EXPECT_EQ(i64 >>= zero, i64);

    EXPECT_EQ(t   >> one, integer(t)   >> 1);
    EXPECT_EQ(f   >> one, integer(f)   >> 1);
    EXPECT_EQ(u8  >> one, integer(u8)  >> 1);
    EXPECT_EQ(u16 >> one, integer(u16) >> 1);
    EXPECT_EQ(u32 >> one, integer(u32) >> 1);
    EXPECT_EQ(u64 >> one, integer(u64) >> 1);
    EXPECT_EQ(i8  >> one, integer(i8)  >> 1);
    EXPECT_EQ(i16 >> one, integer(i16) >> 1);
    EXPECT_EQ(i32 >> one, integer(i32) >> 1);
    EXPECT_EQ(i64 >> one, integer(i64) >> 1);

    EXPECT_EQ(t   >>= one, false);
    EXPECT_EQ(f   >>= one, false);
    EXPECT_EQ(u8  >>= one, (uint8_t)  0x7f);
    EXPECT_EQ(u16 >>= one, (uint16_t) 0x7fff);
    EXPECT_EQ(u32 >>= one, (uint32_t) 0x7fffffff);
    EXPECT_EQ(u64 >>= one, (uint64_t) 0x7fffffffffffffff);
    EXPECT_EQ(i8  >>= one, (int8_t)   0);
    EXPECT_EQ(i16 >>= one, (int16_t)  0);
    EXPECT_EQ(i32 >>= one, (int32_t)  0);
    EXPECT_EQ(i64 >>= one, (int64_t)  0);

    EXPECT_EQ(u8  >> integer(7),  zero);
    EXPECT_EQ(u16 >> integer(15), zero);
    EXPECT_EQ(u32 >> integer(31), zero);
    EXPECT_EQ(u64 >> integer(63), zero);
    EXPECT_EQ(i8  >> integer(7),  zero);
    EXPECT_EQ(i16 >> integer(15), zero);
    EXPECT_EQ(i32 >> integer(31), zero);
    EXPECT_EQ(i64 >> integer(63), zero);

    EXPECT_EQ(u8  >>= integer(7),  (uint8_t)  0);
    EXPECT_EQ(u16 >>= integer(15), (uint16_t) 0);
    EXPECT_EQ(u32 >>= integer(31), (uint32_t) 0);
    EXPECT_EQ(u64 >>= integer(63), (uint64_t) 0);
    EXPECT_EQ(i8  >>= integer(7),  (int8_t)   0);
    EXPECT_EQ(i16 >>= integer(15), (int16_t)  0);
    EXPECT_EQ(i32 >>= integer(31), (int32_t)  0);
    EXPECT_EQ(i64 >>= integer(63), (int64_t)  0);
}

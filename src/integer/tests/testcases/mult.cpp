#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, multiply){
    integer pos ( "fedbca9876543210", 16);
    integer neg ("-fedbca9876543210", 16);

    EXPECT_EQ((pos * pos).str(16),   "fdb8e2bacbfe7cef010e6cd7a44a4100");
    EXPECT_EQ((pos * neg).str(16),  "-fdb8e2bacbfe7cef010e6cd7a44a4100");
    EXPECT_EQ((neg * neg).str(16),   "fdb8e2bacbfe7cef010e6cd7a44a4100");
    EXPECT_EQ((pos *= neg).str(16), "-fdb8e2bacbfe7cef010e6cd7a44a4100");
    EXPECT_EQ((neg *= pos).str(16), "fc9746ea4a27b1546859e0aeb1257bbbbd05a5e419561000");

    const integer zero = 0;
    EXPECT_EQ(pos  * zero, zero);
    EXPECT_EQ(zero * pos,  zero);
    EXPECT_EQ(neg  * zero, zero);
    EXPECT_EQ(zero * neg,  zero);

    const integer one = 1;
    EXPECT_EQ(pos * one, pos);
    EXPECT_EQ(one * pos, pos);
    EXPECT_EQ(neg * one, neg);
    EXPECT_EQ(one * neg, neg);
}

TEST(External, multiply){
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

    EXPECT_EQ(t   *  pos, pos);
    EXPECT_EQ(f   *  pos,   0);
    EXPECT_EQ(u8  *  pos, integer(               "9fffffffffffffffffffffffffffffff60", 16));
    EXPECT_EQ(u16 *  pos, integer(             "a09fffffffffffffffffffffffffffff5f60", 16));
    EXPECT_EQ(u32 *  pos, integer(         "a0a0a09fffffffffffffffffffffffff5f5f5f60", 16));
    EXPECT_EQ(u64 *  pos, integer( "a0a0a0a0a0a0a09fffffffffffffffff5f5f5f5f5f5f5f60", 16));
    EXPECT_EQ(i8  *  pos, integer(              "-50f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0", 16));
    EXPECT_EQ(i16 *  pos, integer(            "-5050f0f0f0f0f0f0f0f0f0f0f0f0f0f0a0a0", 16));
    EXPECT_EQ(i32 *  pos, integer(        "-50505050f0f0f0f0f0f0f0f0f0f0f0f0a0a0a0a0", 16));
    EXPECT_EQ(i64 *  pos, integer("-5050505050505050f0f0f0f0f0f0f0f0a0a0a0a0a0a0a0a0", 16));

    EXPECT_EQ(t   *= pos, true);
    EXPECT_EQ(f   *= pos, false);
    EXPECT_EQ(u8  *= pos, (uint8_t)                0x60);
    EXPECT_EQ(u16 *= pos, (uint16_t)             0x5f60);
    EXPECT_EQ(u32 *= pos, (uint32_t)         0x5f5f5f60);
    EXPECT_EQ(u64 *= pos, (uint64_t) 0x5f5f5f5f5f5f5f60);
    EXPECT_EQ(i8  *= pos, (int8_t)                -0xa0);
    EXPECT_EQ(i16 *= pos, (int16_t)             -0xa0a0);
    EXPECT_EQ(i32 *= pos, (int32_t)         -0xa0a0a0a0);
    EXPECT_EQ(i64 *= pos, (int64_t) -0xa0a0a0a0a0a0a0a0);

    const integer neg = -pos;

    EXPECT_EQ(t   *  neg, neg);
    EXPECT_EQ(f   *  neg,   0);
    EXPECT_EQ(u8  *  neg, integer(              "-5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a00", 16));
    EXPECT_EQ(u16 *  neg, integer(            "-59c3c3c3c3c3c3c3c3c3c3c3c3c3c3c36a00", 16));
    EXPECT_EQ(u32 *  neg, integer(        "-59c32c969696969696969696969696963cd36a00", 16));
    EXPECT_EQ(u64 *  neg, integer("-59c32c95ff68d23c3c3c3c3c3c3c3c3be2790fa63cd36a00", 16));
    EXPECT_EQ(i8  *  neg, integer(              "-5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a00", 16));
    EXPECT_EQ(i16 *  neg, integer(            "-59c3c3c3c3c3c3c3c3c3c3c3c3c3c3c36a00", 16));
    EXPECT_EQ(i32 *  neg, integer(        "-59c32c969696969696969696969696963cd36a00", 16));
    EXPECT_EQ(i64 *  neg, integer("-59c32c95ff68d23c3c3c3c3c3c3c3c3be2790fa63cd36a00", 16));

    EXPECT_EQ(t   *= neg, true);
    EXPECT_EQ(f   *= neg, false);
    EXPECT_EQ(u8  *= neg, (uint8_t)  -0x00);
    EXPECT_EQ(u16 *= neg, (uint16_t) -0x6a00);
    EXPECT_EQ(u32 *= neg, (uint32_t) -0x3cd36a00);
    EXPECT_EQ(u64 *= neg, (uint64_t) -0xe2790fa63cd36a00);
    EXPECT_EQ(i8  *= neg, (int8_t)   -0x00);
    EXPECT_EQ(i16 *= neg, (int16_t)  -0x6a00);
    EXPECT_EQ(i32 *= neg, (int32_t)  -0x3cd36a00);
    EXPECT_EQ(i64 *= neg, (int64_t)  -0xe2790fa63cd36a00);
}

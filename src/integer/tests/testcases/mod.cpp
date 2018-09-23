#include <gtest/gtest.h>

#include "integer.h"

TEST(Arithmetic, modulo){
    // has remainder
    const integer pos       = integer("ffffffffffffffffffffffffffffffffffffffffffffffff", 16);
    const integer pos_mod   = integer("fedcba9876543210",                                 16);
    const integer neg       = -pos;
    const integer neg_mod   = -pos_mod;
    EXPECT_EQ(pos % pos_mod, integer( "888a04f814fda79f", 16));
    EXPECT_EQ(pos % neg_mod, integer( "888a04f814fda79f", 16));
    EXPECT_EQ(neg % pos_mod, integer("-888a04f814fda79f", 16));
    EXPECT_EQ(neg % neg_mod, integer("-888a04f814fda79f", 16));

    // no remainder
    const integer pos_0     = integer("ffffffffffffffffffffffffffffffffffffffffffffffff0", 16);
    const integer pos_mod_0 = integer("ffffffffffffffffffffffffffffffffffffffffffffffff",  16);
    const integer neg_0     = -pos_0;
    const integer neg_mod_0 = -pos_mod_0;
    EXPECT_EQ(pos_0 % pos_mod_0, integer("0", 16));
    EXPECT_EQ(pos_0 % neg_mod_0, integer("0", 16));
    EXPECT_EQ(neg_0 % pos_mod_0, integer("0", 16));
    EXPECT_EQ(neg_0 % neg_mod_0, integer("0", 16));

    // mod 0
    EXPECT_THROW(integer(1) % integer(0), std::domain_error);
}

TEST(External, modulo){
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

    const integer pos("d03", 16); // prime

    EXPECT_EQ(t   %  pos, true);
    EXPECT_EQ(f   %  pos, false);
    EXPECT_EQ(u8  %  pos, integer( "aa",  16));
    EXPECT_EQ(u16 %  pos, integer( "183", 16));
    EXPECT_EQ(u32 %  pos, integer( "249", 16));
    EXPECT_EQ(u64 %  pos, integer( "c7f", 16));
    EXPECT_EQ(i8  %  pos, integer("-56",  16));
    EXPECT_EQ(i16 %  pos, integer("-744", 16));
    EXPECT_EQ(i32 %  pos, integer("-7a7", 16));
    EXPECT_EQ(i64 %  pos, integer("-cc2", 16));

    EXPECT_EQ(t   %= pos, true);
    EXPECT_EQ(f   %= pos, false);
    EXPECT_EQ(u8  %= pos, (uint8_t)   0xaa);
    EXPECT_EQ(u16 %= pos, (uint16_t)  0x183);
    EXPECT_EQ(u32 %= pos, (uint32_t)  0x249);
    EXPECT_EQ(u64 %= pos, (uint64_t)  0xc7f);
    EXPECT_EQ(i8  %= pos, (int8_t)   -0x56);
    EXPECT_EQ(i16 %= pos, (int16_t)  -0x744);
    EXPECT_EQ(i32 %= pos, (int32_t)  -0x7a7);
    EXPECT_EQ(i64 %= pos, (int64_t)  -0xcc2);

    const integer neg = -pos;

    EXPECT_EQ(t   %  neg, integer( "1",   16));
    EXPECT_EQ(f   %  neg, integer( "0",   16));
    EXPECT_EQ(u8  %  neg, integer( "aa",  16));
    EXPECT_EQ(u16 %  neg, integer( "183", 16));
    EXPECT_EQ(u32 %  neg, integer( "249", 16));
    EXPECT_EQ(u64 %  neg, integer( "c7f", 16));
    EXPECT_EQ(i8  %  neg, integer("-56",  16));
    EXPECT_EQ(i16 %  neg, integer("-744", 16));
    EXPECT_EQ(i32 %  neg, integer("-7a7", 16));
    EXPECT_EQ(i64 %  neg, integer("-cc2", 16));

    EXPECT_EQ(t   %= neg, true);
    EXPECT_EQ(f   %= neg, false);
    EXPECT_EQ(u8  %= neg, (uint8_t)   0xaa);
    EXPECT_EQ(u16 %= neg, (uint16_t)  0x183);
    EXPECT_EQ(u32 %= neg, (uint32_t)  0x249);
    EXPECT_EQ(u64 %= neg, (uint64_t)  0xc7f);
    EXPECT_EQ(i8  %= neg, (int8_t)   -0x56);
    EXPECT_EQ(i16 %= neg, (int16_t)  -0x744);
    EXPECT_EQ(i32 %= neg, (int32_t)  -0x7a7);
    EXPECT_EQ(i64 %= neg, (int64_t)  -0xcc2);
}
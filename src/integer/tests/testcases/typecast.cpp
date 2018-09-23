#include <gtest/gtest.h>

#include "integer.h"

TEST(Typecast, all){
    const integer pos("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 16);

    EXPECT_EQ(static_cast <bool>     (integer(true)),            true);
    EXPECT_EQ(static_cast <bool>     (integer(false)),           false);
    EXPECT_EQ(static_cast <uint8_t>  (pos),           (uint8_t)  0xaa);
    EXPECT_EQ(static_cast <uint16_t> (pos),           (uint16_t) 0xaaaa);
    EXPECT_EQ(static_cast <uint32_t> (pos),           (uint32_t) 0xaaaaaaaa);
    EXPECT_EQ(static_cast <uint64_t> (pos),           (uint64_t) 0xaaaaaaaaaaaaaaaa);
    EXPECT_EQ(static_cast <int8_t>   (pos),           (int8_t)   0xaa);
    EXPECT_EQ(static_cast <int16_t>  (pos),           (int16_t)  0xaaaa);
    EXPECT_EQ(static_cast <int32_t>  (pos),           (int32_t)  0xaaaaaaaa);
    EXPECT_EQ(static_cast <int64_t>  (pos),           (int64_t)  0xaaaaaaaaaaaaaaaa);

    const integer neg = -pos;
    EXPECT_EQ(static_cast <bool>     (integer(true)),             true);
    EXPECT_EQ(static_cast <bool>     (integer(false)),            false);
    EXPECT_EQ(static_cast <uint8_t>  (neg),           (uint8_t)   0x56);
    EXPECT_EQ(static_cast <uint16_t> (neg),           (uint16_t)  0x5556);
    EXPECT_EQ(static_cast <uint32_t> (neg),           (uint32_t)  0x55555556);
    EXPECT_EQ(static_cast <uint64_t> (neg),           (uint64_t)  0x5555555555555556);
    EXPECT_EQ(static_cast <int8_t>   (neg),           (int8_t)   -0xaa);
    EXPECT_EQ(static_cast <int16_t>  (neg),           (int16_t)  -0xaaaa);
    EXPECT_EQ(static_cast <int32_t>  (neg),           (int32_t)  -0xaaaaaaaa);
    EXPECT_EQ(static_cast <int64_t>  (neg),           (int64_t)  -0xaaaaaaaaaaaaaaaa);
}
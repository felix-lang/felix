#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Constructor, standard){
    uint256_t value(0x0123456789abcdefULL);
    const uint256_t original = value;

    EXPECT_EQ(uint256_t(), 0);
    EXPECT_EQ(value, original);
    EXPECT_EQ(uint256_t(std::move(value)), original);
    EXPECT_EQ(value, 0);
}

TEST(Constructor, one){
    EXPECT_EQ(uint256_t(true).upper(),  false);
    EXPECT_EQ(uint256_t(true).lower(),   true);
    EXPECT_EQ(uint256_t(false).upper(), false);
    EXPECT_EQ(uint256_t(false).lower(), false);

    EXPECT_EQ(uint256_t((uint8_t)  0x01ULL).upper(),               0ULL);
    EXPECT_EQ(uint256_t((uint16_t) 0x0123ULL).upper(),             0ULL);
    EXPECT_EQ(uint256_t((uint32_t) 0x01234567ULL).upper(),         0ULL);
    EXPECT_EQ(uint256_t((uint64_t) 0x0123456789abcdefULL).upper(), 0ULL);

    EXPECT_EQ(uint256_t((uint8_t)  0x01ULL).lower(),               (uint8_t)  0x01ULL);
    EXPECT_EQ(uint256_t((uint16_t) 0x0123ULL).lower(),             (uint16_t) 0x0123ULL);
    EXPECT_EQ(uint256_t((uint32_t) 0x01234567ULL).lower(),         (uint32_t) 0x01234567ULL);
    EXPECT_EQ(uint256_t((uint64_t) 0x0123456789abcdefULL).lower(), (uint64_t) 0x0123456789abcdefULL);
}

TEST(Constructor, two){
    for(uint8_t hi = 0; hi < 2; hi++){
        for(uint8_t lo = 0; lo < 2; lo++){
            const uint256_t val(hi, lo);
            EXPECT_EQ(val.upper(), hi);
            EXPECT_EQ(val.lower(), lo);
        }
    }

    EXPECT_EQ(uint256_t((uint8_t)  0x01ULL,               (uint8_t)  0x01ULL).upper(),               (uint8_t)  0x01ULL);
    EXPECT_EQ(uint256_t((uint16_t) 0x0123ULL,             (uint16_t) 0x0123ULL).upper(),             (uint16_t) 0x0123ULL);
    EXPECT_EQ(uint256_t((uint32_t) 0x01234567ULL,         (uint32_t) 0x01234567ULL).upper(),         (uint32_t) 0x01234567ULL);
    EXPECT_EQ(uint256_t((uint64_t) 0x0123456789abcdefULL, (uint64_t) 0x0123456789abcdefULL).upper(), (uint64_t) 0x0123456789abcdefULL);

    EXPECT_EQ(uint256_t((uint8_t)  0x01ULL,               (uint8_t)  0x01ULL).lower(),               (uint8_t)  0x01ULL);
    EXPECT_EQ(uint256_t((uint16_t) 0x0123ULL,             (uint16_t) 0x0123ULL).lower(),             (uint16_t) 0x0123ULL);
    EXPECT_EQ(uint256_t((uint32_t) 0x01234567ULL,         (uint32_t) 0x01234567ULL).lower(),         (uint32_t) 0x01234567ULL);
    EXPECT_EQ(uint256_t((uint64_t) 0x0123456789abcdefULL, (uint64_t) 0x0123456789abcdefULL).lower(), (uint64_t) 0x0123456789abcdefULL);
}

TEST(Constructor, four){
    for(uint8_t hi_hi = 0; hi_hi < 2; hi_hi++){
        for(uint8_t hi_lo = 0; hi_lo < 2; hi_lo++){
            for(uint8_t lo_hi = 0; lo_hi < 2; lo_hi++){
                for(uint8_t lo_lo = 0; lo_lo < 2; lo_lo++){
                    const uint256_t val(hi_hi, hi_lo, lo_hi, lo_lo);
                    EXPECT_EQ(val.upper().upper(), hi_hi);
                    EXPECT_EQ(val.upper().lower(), hi_lo);
                    EXPECT_EQ(val.lower().upper(), lo_hi);
                    EXPECT_EQ(val.lower().lower(), lo_lo);
                }
            }
        }
    }
}

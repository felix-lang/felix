#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(BitWise, invert){
    for(uint8_t hi_hi = 0; hi_hi < 2; hi_hi++){
        for(uint8_t hi_lo = 0; hi_lo < 2; hi_lo++){
            for(uint8_t lo_hi = 0; lo_hi < 2; lo_hi++){
                for(uint8_t lo_lo = 0; lo_lo < 2; lo_lo++){
                    const uint256_t val = ~uint256_t(hi_hi?0xffffffffffffffffULL:0x0000000000000000ULL,
                                                     hi_lo?0xffffffffffffffffULL:0x0000000000000000ULL,
                                                     lo_hi?0xffffffffffffffffULL:0x0000000000000000ULL,
                                                     lo_lo?0xffffffffffffffffULL:0x0000000000000000ULL);

                    EXPECT_EQ(val.upper().upper(), hi_hi?0x0000000000000000ULL:0xffffffffffffffffULL);
                    EXPECT_EQ(val.upper().lower(), hi_lo?0x0000000000000000ULL:0xffffffffffffffffULL);
                    EXPECT_EQ(val.lower().upper(), lo_hi?0x0000000000000000ULL:0xffffffffffffffffULL);
                    EXPECT_EQ(val.lower().lower(), lo_lo?0x0000000000000000ULL:0xffffffffffffffffULL);
                }
            }
        }
    }
}
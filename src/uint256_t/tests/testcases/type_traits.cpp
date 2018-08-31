#include <type_traits>

#include <gtest/gtest.h>

#include "uint256_t.h"

TEST(Type_Traits, is_arithmetic){
    EXPECT_EQ(std::is_arithmetic <uint256_t>::value, true);
}

TEST(Type_Traits, is_integral){
    EXPECT_EQ(std::is_integral <uint256_t>::value, true);
}

TEST(Type_Traits, is_unsigned){
    EXPECT_EQ(std::is_unsigned <uint256_t>::value, true);
}

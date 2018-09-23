#include <type_traits>

#include <gtest/gtest.h>

#include "integer.h"

TEST(Type_Traits, is_arithmetic){
    EXPECT_EQ(std::is_arithmetic <integer>::value, true);
}

TEST(Type_Traits, is_integral){
    EXPECT_EQ(std::is_integral <integer>::value, true);
}

TEST(Type_Traits, is_signed){
    EXPECT_EQ(std::is_signed <integer>::value, true);
}

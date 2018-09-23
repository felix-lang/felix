#include <gtest/gtest.h>

#include "integer.h"

TEST(Accessor, sign){
    EXPECT_EQ( integer("0",     16).sign(), integer::POSITIVE);
    EXPECT_EQ( integer("12345", 16).sign(), integer::POSITIVE);
    EXPECT_EQ(integer("-12345", 16).sign(), integer::NEGATIVE);
}

TEST(Accessor, bits){
    integer value = 1;
    for(uint32_t i = 0; i < 255; i++){
        EXPECT_EQ(value.bits(), i + 1);                     // before shift
        value <<= 1;
    }

    EXPECT_EQ(integer(0).bits(), (integer::REP_SIZE_T) 0);
}

TEST(Accessor, bytes){
    integer value = 1;

    for(uint32_t i = 0; i < 256; i++){
        EXPECT_EQ(value.bytes(), (i >> 3) + 1); // before shift
        value <<= 1;
    }

    EXPECT_EQ(integer(0).bytes(), (integer::REP_SIZE_T) 0);
}

TEST(Accessor, digits){
    const std::string str = "fedcba9876543210";
    const integer value(str, 16);
    const integer::REP_SIZE_T digits = 8 / sizeof(INTEGER_DIGIT_T);

    EXPECT_EQ( value.digits(),  digits);
    EXPECT_EQ((-value).digits(), digits);

    EXPECT_EQ(integer().digits(),  (integer::REP_SIZE_T) 0);
    EXPECT_EQ(integer(0).digits(), (integer::REP_SIZE_T) 0);
}

TEST(Accessor, data){
    const integer value("fedcba9876543210", 16);
    integer::REP data;

    // this should be a macro
    if (std::is_same <INTEGER_DIGIT_T, uint8_t>::value){
        data = {(INTEGER_DIGIT_T) 0xfe,
                (INTEGER_DIGIT_T) 0xdc,
                (INTEGER_DIGIT_T) 0xba,
                (INTEGER_DIGIT_T) 0x98,
                (INTEGER_DIGIT_T) 0x76,
                (INTEGER_DIGIT_T) 0x54,
                (INTEGER_DIGIT_T) 0x32,
                (INTEGER_DIGIT_T) 0x10};
    }
    else if (std::is_same <INTEGER_DIGIT_T, uint16_t>::value){
        data = {(INTEGER_DIGIT_T) 0xfedc,
                (INTEGER_DIGIT_T) 0xba98,
                (INTEGER_DIGIT_T) 0x7654,
                (INTEGER_DIGIT_T) 0x3210};
    }
    else if (std::is_same <INTEGER_DIGIT_T, uint32_t>::value){
        data = {(INTEGER_DIGIT_T) 0xfedcba98,
                (INTEGER_DIGIT_T) 0x76543210};
    }
    else if (std::is_same <INTEGER_DIGIT_T, uint64_t>::value){
        data = {(INTEGER_DIGIT_T) 0xfedcba9876543210};
    }

    EXPECT_EQ( value.data(),  data);
    EXPECT_EQ((-value).data(), data);
}

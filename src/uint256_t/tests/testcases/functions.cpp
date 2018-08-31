#include <map>

#include <gtest/gtest.h>

#include "uint256_t.h"

static const std::map <uint32_t, std::string> tests = {
    std::make_pair(2,  "10000100000101011000010101101100"),
    std::make_pair(3,  "12201102210121112101"),
    std::make_pair(4,  "2010011120111230"),
    std::make_pair(5,  "14014244043144"),
    std::make_pair(6,  "1003520344444"),
    std::make_pair(7,  "105625466632"),
    std::make_pair(8,  "20405302554"),
    std::make_pair(9,  "5642717471"),
    std::make_pair(10, "2216002924"),
    std::make_pair(11, "a3796a883"),
    std::make_pair(12, "51a175124"),
    std::make_pair(13, "294145645"),
    std::make_pair(14, "170445352"),
    std::make_pair(15, "ce82d6d4"),
    std::make_pair(16, "8415856c"),
    std::make_pair(17, "56dc4e33"),
    std::make_pair(18, "3b2db13a"),
    std::make_pair(19, "291i3b4g"),
    std::make_pair(20, "1eca0764"),
    std::make_pair(21, "14hc96jg"),
    std::make_pair(22, "jblga9e"),
    std::make_pair(23, "em6i5a5"),
    std::make_pair(24, "be75374"),
    std::make_pair(25, "91mo4go"),
    std::make_pair(26, "74d74li"),
    std::make_pair(27, "5jblgea"),
    std::make_pair(28, "4gl7i9g"),
    std::make_pair(29, "3l13lor"),
    std::make_pair(30, "315o5e4"),
    std::make_pair(31, "2fcfub9"),
    std::make_pair(32, "221b1bc"),
    std::make_pair(33, "1nkji2p"),
    std::make_pair(34, "1eq93ik"),
    std::make_pair(35, "176p6y9"),
    std::make_pair(36, "10ncmss")
    // std::make_pair(256, "uint256_t"),
};

TEST(Function, str){
    // number of leading 0s
    const std::string::size_type leading = 5;

    // make sure all of the test strings create the ASCII version of the string
    const uint256_t original(2216002924);
    for(std::pair <uint32_t const, std::string>  t : tests){
        EXPECT_EQ(original.str(t.first), t.second);
    }

    // add leading zeros
    for(uint32_t base = 2; base <= 36; base++){
        EXPECT_EQ(original.str(base, tests.at(base).size() + leading), std::string(leading, '0') + tests.at(base));
    }
}

TEST(External, ostream){
    const uint256_t value(0xfedcba9876543210ULL);

    // write out octal uint256_t
    std::stringstream oct; oct << std::oct << value;
    EXPECT_EQ(oct.str(), "1773345651416625031020");

    // write out decimal uint256_t
    std::stringstream dec; dec << std::dec << value;
    EXPECT_EQ(dec.str(), "18364758544493064720");

    // write out hexadecimal uint256_t
    std::stringstream hex; hex << std::hex << value;
    EXPECT_EQ(hex.str(), "fedcba9876543210");

    // zero
    std::stringstream zero; zero << uint256_t();
    EXPECT_EQ(zero.str(), "0");
}

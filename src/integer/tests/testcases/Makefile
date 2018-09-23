# integer testcases Makefile
CXX?=g++
CXXFLAGS=-std=c++11 -Wall -g -I../../../googletest/googletest/include -I../..

include objects.mk

all: $(INTEGER_TESTCASES_OBJECTS)

.PHONY: clean

$(INTEGER_TESTCASES_OBJECTS): %.o : %.cpp ../../integer.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(INTEGER_TESTCASES_OBJECTS)

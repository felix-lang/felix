#include "slist.hpp"
#include <iostream>
using namespace Felix;

int main() {
 std::cout << "Hello, slist test" << std::endl;
 auto base = slist<int>();
 for (int i=0; i<10; ++i) {
   base = cons(i,base);
 }
 while (!base.empty()) {
   std::cout << base.head() << std::endl;
   base = base.tail();
 }
 std::cout << "Bye, slist test" << std::endl;
   
}
 

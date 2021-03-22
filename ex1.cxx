#include "flx_rtl.hpp"
#include "flx_executil.hpp"
#include <iostream>

using namespace flx;
using namespace rtl;

struct diagnostic: public con_t {
  int number;
  con_t *call (con_t* callr, int x) { 
    _caller = callr; number = x; 
    return this;
  }
  con_t *resume() {
    ::std::cout<< "Number " << number << ::std::endl;
    return _caller;
  }
};

struct doit: public con_t {
  int counter;
  diagnostic *tmp;
  con_t *call() { FLX_RESETPC counter = 1; return this; }
  con_t *resume() {
    FLX_START_SWITCH
        tmp = new diagnostic;
    again:
        tmp->call(this,counter);
        FLX_SET_PC(1)
        return tmp;
      FLX_CASE_LABEL(1)
        ++counter;
        if(counter <= 10) goto again;   
        delete tmp;  
      FLX_RETURN
    FLX_END_SWITCH
  }
};

void testit() {
con_t *cc = (new doit)->call();
flx::rtl::executil::run (cc);
delete cc;
}

int main() {
  testit();
}

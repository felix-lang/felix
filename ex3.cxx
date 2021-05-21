// TEST CASE
#include "sync3.hpp"
#include <list>
struct producer : con_t {
  ::std::list<int> *plst;
  ::std::list<int>::iterator it;
  channel_t *chan;
  union {
    void *iodata;
    int value;
  };
  io_request_t w_req;

  con_t *call(
    con_t *caller_a, 
    ::std::list<int> *plst_a,
    channel_t *outchan)
  { 
    caller = caller_a;
    plst = plst_a;
    pc = 0;
    w_req.chan = outchan;
    return this;
  }

  con_t *resume() override {
    switch (pc) {
      case 0:
        it = plst->begin();
        pc = 1;
        w_req.svc_code = write_request_code_e;
        w_req.pdata = &iodata;
        return this;

      case 1:
        if(it == plst->end()) { 
          auto tmp = caller; 
          delete this;
          return caller; 
        }
        value = *it++;
        svc_req = (svc_req_t*)(void*)&w_req; // service request
        return this;
      default: assert(false);
    }
  }
};

struct consumer: con_t {
  ::std::list<int> *plst;
  union {
    void *iodata;
    int value;
  };
  io_request_t r_req;

  con_t *call(
    con_t *caller_a, 
    ::std::list<int> *plst_a,
    channel_t *inchan_a)
  { 
    caller = caller_a;
    plst = plst_a;
    r_req.chan = inchan_a;
    pc = 0;
    return this;
  }

  con_t *resume() override {
    switch (pc) {
      case 0:
        pc = 1;
        r_req.svc_code = read_request_code_e;
        r_req.pdata = &iodata;
        return this;

      case 1:
        svc_req = (svc_req_t*)(void*)&r_req; // service request
        pc = 2;
        return this;

      case 2:
        plst->push_back(value);
        pc = 1;
        return this;
      default: assert(false);
    }
  }
};

struct transducer: con_t {
  union {
    void *iodata;
    int value;
  };
  io_request_t r_req;
  io_request_t w_req;

  con_t *call(
    con_t *caller_a, 
    channel_t *inchan_a,
    channel_t *outchan_a)
  { 
    caller = caller_a;
    r_req.chan = inchan_a;
    w_req.chan = outchan_a;
    pc = 0;
    return this;
  }

  con_t *resume() override {
    switch (pc) {
      case 0:
        pc = 1;
        r_req.svc_code = read_request_code_e;
        r_req.pdata = &iodata;
        w_req.svc_code = write_request_code_e;
        w_req.pdata = &iodata;
        return this;

      case 1:
        svc_req = (svc_req_t*)(void*)&r_req; // service request
        pc = 2;
        return this;

      case 2:
        value = value * value; // square value
        svc_req = (svc_req_t*)(void*)&w_req; // service request
        pc = 1;
        return this;
      default: assert(false);
    }
  }
};

#include <iostream>

int main() {
  // create the input list
  ::std::list<int> inlst;
  for (auto i = 0; i < 20; ++i) inlst.push_back(i);

  // output list
  ::std::list<int> outlst;

  // create scheduler
  sync_sched sched;

  // create channels
  channel_t chan1;
  channel_t chan2;

  // create fibres
  fibre_t *prod = new fibre_t ((new producer)->call(nullptr, &inlst, &chan1));
  fibre_t *trans = new fibre_t ((new transducer)->call(nullptr, &chan1, &chan2));
  fibre_t *cons = new fibre_t ((new consumer)->call(nullptr, &outlst, &chan2));

  // push initial fibres onto scheduler active list
  sched.active_set->push_new(prod);
  sched.active_set->push_new(trans);
  sched.active_set->push_new(cons);
 
  // run it
  sched.sync_run();

::std::fflush(stdout);

  // the result is now in the outlist so print it
  ::std::cout<< "List of squares:" << ::std::endl;
  for(auto v : outlst) ::std::cout << v << ::std::endl;
} 


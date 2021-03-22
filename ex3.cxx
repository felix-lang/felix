
// EX3
#include <cstdint>
#include <cassert>
#include <cstdio>

// continuation
struct con_t {
  con_t *caller; // caller continuation
  int pc;        // program counter
  union svc_req_t *svc_req; // request
  virtual con_t *resume()=0;
  virtual ~con_t(){}
};

// fibre
struct fibre_t {
  con_t *cc;
  fibre_t *next;
  active_set_t *owner;

  // default DEAD
  fibre_t() : cc(nullptr), next(nullptr) {}

  // construct from continuation
  fibre_t(con_t *ccin) : cc(ccin), next (nullptr) {}

  // immobile
  fibre_t(fibre_t const&)=delete;
  fibre_t& operator=(fibre_t const&)=delete;

  // destructor deletes any remaining continuations in spaghetti stack
  ~fibre_t() {
    while(cc) {
      con_t *tmp = cc->caller;
      delete cc;
      cc = tmp;
    }
  }
 
  // run until either fibre issues a service request or dies
  svc_req_t *run_fibre() { 
    while(cc) {
      cc=cc->resume(); 
      if(cc && cc->svc_req) return cc->svc_req;
    }
    return nullptr;
  }
};

// low bit fiddling routines
inline static bool get_lowbit(void *p) { 
  return (uintptr_t)p & (uintptr_t)1u; 
}
inline static void *clear_lowbit(void *p) { 
  return (void*)((uintptr_t)p & ~(uintptr_t)1u); 
}
inline static void *set_lowbit(void *p) { 
  return (void*)((uintptr_t)p | (uintptr_t)1u); 
}

// channel
struct channel_t {
  fibre_t *top;
  ::std::atomic_flag lock;
  
  channel_t () : top (nullptr), lock(ATOMIC_FLAG_INIT) {}

  // destructor deletes all continuations left in channel
  ~channel_t() {
    while (top) {
      fibre_t *f = (fibre_t*)clear_lowbit(top);
      fibre_t *tmp = f->next;
      delete f;
      top = tmp;
    }
  }

  // push a fibre as a reader: precondition it must be a reader
  // and, if the channel is non-empty it must contain only readers
  void push_reader(fibre_t *r) { 
    r->next = top; 
    top = r; 
  }

  // push a fibre as a writer: precondition it must be a writer
  // and, if the channel is non-empty it must contain only writers
  void push_writer(fibre_t *w) { 
    w->next = top; 
    top = (fibre_t*)set_lowbit(w);
  }

  // pop a reader if there is one, otherwise nullptr
  fibre_t *pop_reader() { 
    fibre_t *tmp = top; 
    if(get_lowbit(tmp))return nullptr;
    top = top -> next;
    return tmp; // lowbit is clear, its a reader 
  }

  // pop a writer if there is one, otherwise nullptr
  fibre_t *pop_writer() { 
    fibre_t *tmp = top; 
    if(!get_lowbit(tmp)) return nullptr;
    tmp = (fibre_t*)clear_lowbit(tmp); // lowbit is set for writer
    top = tmp -> next;
    return tmp;
  }
};

// active set
struct active_set_t {
  ::std::atomic_size_t refcnt;
  fibre_t *active;
  ::std::atomic_flag lock;
  active_set() : refcnt(1), active(nullptr), lock(ATOMIC_FLAG_INIT) {}

  active_set_t *share() { ++refcnt; return this; }
  void forget() { --refcnt; if(!atomic_load(&refcnt)) delete this; }

  // push a new active fibre onto active list
  void push(fibre_t *fresh) { 
    while(lock.test_and_set(::std::memory_order_acquire); // spin
    fresh->next = active; 
    active = fresh; 
    lock.clear(::std::memory_order_release); // release lock
  }
  // pop an active fibre off the active list
  fibre_t *pop() {
    while(lock.test_and_set(::std::memory_order_acquire); // spin
    fibre_t *tmp = active;
    if(tmp)active = tmp->next;
    lock.clear(::std::memory_order_release); // release lock
    return tmp;
  }
};

// service requests
enum svc_code_t {
  read_request_code_e,
  write_request_code_e,
  spawn_fibre_request_code_e
};
struct io_request_t {
  svc_code_t svc_code;
  channel_t *chan;
  void **pdata;
};
struct spawn_fibre_request_t {
  svc_code_t svc_code;
  fibre_t *tospawn;
};
union svc_req_t {
  io_request_t io_request;
  spawn_fibre_request_t spawn_fibre_request;
  svc_code_t get_code () const { return io_request.svc_code; }
};

// scheduler
struct sync_sched {
  fibre_t *current; // currently running fibre, nullptr if none
  active_set_t *active_set;  // chain of fibres ready to run

  sync_sched() : current(nullptr), active(nullptr) {}

  void sync_run();
  void do_read(io_request_t *req);
  void do_write(io_request_t *req);
  void do_spawn_fibre(spawn_fibre_request_t *req);
};

// scheduler subroutine runs until there is no work to do
void sync_sched::sync_run() {
  current = pop(); // get some work
  while(current) // while there's work to do 
  {
    svc_req_t *svc_req = current->run_fibre();
    if(svc_req)  // fibre issued service request
      switch (svc_req->get_code()) 
      {
        case read_request_code_e: 
          do_read(&(svc_req->io_request));
          break;
        case write_request_code_e:  
          do_write(&(svc_req->io_request));
          break;
        case spawn_fibre_request_code_e:  
          do_spawn_fibre(&(svc_req->spawn_fibre_request));
          break;
      }
    else // the fibre returned without issuing a request so should be dead
    {
      assert(!current->cc); // check it's adead fibre
      delete current;       // delete dead fibre
      current = pop();      // get more work
    }
  }
}


void sync_sched::do_read(io_request_t *req) {
  while(lock.test_and_set(::std::memory_order_acquire); // spin
  fibre_t *w = req->chan->pop_writer();
  if(w) {
    lock.clear(::std::memory_order_release); // release lock
    *current->cc->svc_req->io_request.pdata =
      *w->cc->svc_req->io_request.pdata; // transfer data

    // null out svc requests so they're not re-issued
    w->cc->svc_req = nullptr;
    current->cc->svc_req = nullptr;

    w->owner.push(w); // onto active list
    // i/o match: reader retained as current
  }
  else {
    req->chan->push_reader(current);
    lock.clear(::std::memory_order_release); // release lock
    current = pop(); // active list
    // i/o fail: current pushed then set to next active
  }
}

void sync_sched::do_write(io_request_t *req) {
  while(lock.test_and_set(::std::memory_order_acquire); // spin
  fibre_t *r = req->chan->pop_reader();
  if(r) {
    lock.clear(::std::memory_order_release); // release lock
    *r->cc->svc_req->io_request.pdata = 
      *current->cc->svc_req->io_request.pdata; // transfer data

    // null out svc requests so they're not re-issued
    r->cc->svc_req = nullptr;
    current->cc->svc_req = nullptr;

    if(r->owner == active_set) {
      active_set->push(current); // current is writer, pushed onto active list
      current = r; // make reader current
    }
    else {
      r->owner.push(r);
      // writer remains current if reader is foreign
    }
  }
  else {
    req->chan->push_writer(current); // i/o fail: push current onto channel
    lock.clear(::std::memory_order_release); // release lock
    current = active_set->pop(); // reset current from active list
  }
}


void sync_sched::do_spawn_fibre(spawn_fibre_request_t *req) {
  current->cc->svc_req=nullptr;
  push(current);
  current = req->tospawn;
}

// TEST CASE
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
  sched.push(prod);
  sched.push(trans);
  sched.push(cons);
 
  // run it
  sched.sync_run();

::std::fflush(stdout);

  // the result is now in the outlist so print it
  ::std::cout<< "List of squares:" << ::std::endl;
  for(auto v : outlst) ::std::cout << v << ::std::endl;
} 


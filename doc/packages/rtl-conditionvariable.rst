Package: src/packages/rtl-conditionvariable.fdoc


==================
Condition Variable
==================


================= ===================================
key               file                                
================= ===================================
pthread_condv.hpp share/lib/rtl/pthread_condv.hpp     
pthread_condv.cpp share/src/pthread/pthread_condv.cpp 
================= ===================================



Condition Variable
==================


.. index:: PTHREAD_EXTERN(class)
.. code-block:: cpp

  //[pthread_condv.hpp]
  #ifndef __FLX_PTHREAD_CONDV_HPP__
  #define __FLX_PTHREAD_CONDV_HPP__
  #include <condition_variable>
  #include <chrono>
  #include "flx_pthread_config.hpp"
  #include "pthread_thread_control_base.hpp"
  
  namespace flx { namespace pthread {
  class PTHREAD_EXTERN flx_condv_t : public world_stop_notifier_t
  {
    ::std::mutex m;
    ::std::condition_variable_any cv;
    void notify_world_stop() override;
    thread_control_base_t *tc;
  public:
     flx_condv_t (thread_control_base_t *);
     void lock();
     void unlock();
     void wait();
     void timed_wait(double seconds);
     void signal();
     void broadcast();
     ~flx_condv_t();
  };
  
  }}
  #endif

.. code-block:: cpp

  //[pthread_condv.cpp]
  #include "pthread_condv.hpp"
  #include <stdint.h>
  
  namespace flx { namespace pthread {
  // constructor
  flx_condv_t::flx_condv_t(thread_control_base_t *tc_): tc(tc_) { 
  //fprintf(stderr, "Creating condition variable %p\n", this);
    tc->register_world_stop_notifier(this); 
  }
  
  void flx_condv_t::notify_world_stop() { cv.notify_all(); }
  
  void flx_condv_t::lock() { m.lock(); }
  
  void flx_condv_t::unlock() { m.unlock(); }
  
  // mutex must be LOCKED on entry to WAIT
  // mutex will be LOCKED on exit from WAIT
  void flx_condv_t::wait() {
    m.unlock();
    tc->yield();
    m.lock();
    cv.wait_for(m,::std::chrono::seconds (1));  // unlocks mutex on entry, relocks on exit
  }
  
  void flx_condv_t::timed_wait(double seconds) {
    m.unlock();
    tc->yield();
    m.lock();
    cv.wait_for(m,::std::chrono::microseconds ((uint64_t)(seconds*1000000.0)));
  }
  
  void flx_condv_t::signal() { cv.notify_one(); }
  
  void flx_condv_t::broadcast() { cv.notify_all(); }
  
  flx_condv_t::~flx_condv_t() { tc->unregister_world_stop_notifier(this); }
  
  }}




Package: src/packages/rtl-waitablebool.fdoc


=============
Waitable bool
=============

========================= ===========================================
key                       file                                        
========================= ===========================================
pthread_waitable_bool.hpp share/lib/rtl/pthread_waitable_bool.hpp     
pthread_waitable_bool.cpp share/src/pthread/pthread_waitable_bool.cpp 
========================= ===========================================


Shared Boolean
==============

Used by demux_quitter

.. index:: PTHREAD_EXTERN(class)
.. code-block:: cpp

  //[pthread_waitable_bool.hpp]
  #ifndef __FLX_PTHREAD_WAIT_BOOL_H__
  #define __FLX_PTHREAD_WAIT_BOOL_H__
  #include "flx_pthread_config.hpp"
  #include <thread>
  #include <mutex>
  #include <condition_variable>
  
  namespace flx { namespace pthread {
  
  // a waitable boolean.
  class PTHREAD_EXTERN waitable_bool {
    ::std::mutex cv_lock;       // to work with the condition var
    ::std::condition_variable_any finished_cond;
    bool finished;   // might seem redundant, but that's how CVs work.
  public:
    waitable_bool();
  
    void wait_until_true();
    void signal_true();
  };
  
  }} // namespace pthread, flx
  #endif // __FLX_PTHREAD_WAIT_BOOL_H__
  

.. code-block:: cpp

  //[pthread_waitable_bool.cpp]
  #include "pthread_waitable_bool.hpp"
  
  namespace flx { namespace pthread {
  
  waitable_bool::waitable_bool()
    : finished(false)
  {
    // nothing
  }
  
  // can be called from any thread
  void
  waitable_bool::wait_until_true()
  {
    ::std::unique_lock< ::std::mutex> locker(cv_lock);
  
    // wait for the wakeup to say it's finished
    while(!finished)
    {
      finished_cond.wait(cv_lock);
    }
  }
  
  void
  waitable_bool::signal_true()
  {
    { // the mutex is required for the memory barrier..
      ::std::unique_lock< ::std::mutex> locker(cv_lock);
      finished = true;
    }
    finished_cond.notify_all();
    // do absolutely NOTHING here as a typical use of this class is to
    // wait for a thread exit and then destruct its resources, which could
    // very well include this object. boom.
  }
  
  } }
  
  
  

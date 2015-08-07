#ifndef __FLX_PTHREAD_THREAD_H__
#define __FLX_PTHREAD_THREAD_H__
#include "flx_pthread_config.hpp"

#if FLX_WIN32
#include <windows.h>
#else
#include <pthread.h>
#endif

// auto pthread, because I forget how to deallocate them nicely
// could init in the constructor, but ultimately you don't want the thread
// barging in before you've finished doing other stuff
// Addendum (20051128): doing stdio in turns out to be not very safe.
// I don't know if printf et al are supposed to be thread safe (most impls
// seem to try to be) but I sometimes get deadlocks in ppc64 os x 10.4.2
// with 4.0.1 when printfing to stdout. Nasty.

#include "pthread_mutex.hpp"
#include "pthread_condv.hpp"

#include <utility>
#include <map>
#include <vector>
#include <functional>

namespace flx { namespace pthread {

// ********************************************************
/// Posix Threads. This class simply wraps the creation
/// and joining of threads. It is not safe.
// ********************************************************

#ifdef _WIN32
typedef HANDLE flx_native_thread_t;
#else
typedef pthread_t flx_native_thread_t;
#endif

flx_native_thread_t PTHREAD_EXTERN get_current_native_thread();

struct thread_data_t {
  thread_data_t(void *b) : stack_base(b), stack_top(0), active(true) {}
  void *stack_base;
  void *stack_top;
  bool active;
};

struct memory_range_t {
  memory_range_t(void *b_, void *e_) : b(b_), e(e_) {}
  void *b;
  void *e;
};

typedef std::pair<flx_native_thread_t const, thread_data_t> thread_entry_t;
typedef std::map<flx_native_thread_t, thread_data_t, std::less<flx_native_thread_t> > thread_registry_t;
typedef std::vector<memory_range_t> memory_ranges_t;

class PTHREAD_EXTERN thread_control_t
{
    thread_control_t (thread_control_t const &); // uncopyable
    void operator=(thread_control_t const&); // uncopyable
    bool do_world_stop;
    unsigned int thread_counter;
    unsigned int active_counter;
    flx_condv_t stop_guard;
    flx_mutex_t stop_mutex;
    thread_registry_t threads;
    void unsafe_stop_check();
    void unsafe_suspend();
    void unsafe_resume();
public:
    bool debug;
    thread_control_t (bool);
    int thread_count();
    int active_count();
    void add_thread(void*);
    void remove_thread();
    bool world_stop();
    void join_all() ;
    void world_start();
    void yield();
    void suspend();
    void resume();
    memory_ranges_t *get_block_list(); // called owns result and should delete it
};

struct tstart_t
{
  void (*sr)(void*);
  void *cd;
  thread_control_t *tc;
  flx_mutex_t *spawner_lock;
  flx_condv_t *spawner_cond;
  bool *spawner_flag;

  tstart_t(void (*s)(void*),void* c,thread_control_t *t, flx_mutex_t *sl, flx_condv_t *sc, bool *sf)
    : sr(s), cd(c), tc(t), spawner_lock(sl), spawner_cond(sc), spawner_flag(sf)
  {}
};

// a class for threads that can't be joined. upon exit all their resources
// are freed. they just evaporate. probably the best type of thread.
class PTHREAD_EXTERN flx_detached_thread_t {
  flx_native_thread_t thr;        ///< the thread
  flx_detached_thread_t(flx_detached_thread_t const&); // uncopyable
  void operator=(flx_detached_thread_t const&); // uncopyable
public:
  flx_detached_thread_t();
  ~flx_detached_thread_t();
  int init(void (*start)(void*), void* udat, thread_control_t*, flx_mutex_t*, flx_condv_t*, bool*);
};

// rf: joinable threads. is it an error to not join joinable threads?
class PTHREAD_EXTERN flx_thread_t {
  flx_native_thread_t thr;        ///< the thread
  flx_thread_t(flx_thread_t const&); // uncopyable
  void operator=(flx_thread_t const&); // uncopyable
public:
  flx_thread_t();
  ~flx_thread_t();
  int init(void (*start)(void*), void* udat, thread_control_t*);
  void join();
};

/// RAII wrapper for thread class
class PTHREAD_EXTERN flx_thread_wrapper_t {
  flx_thread_t thread;
  flx_thread_wrapper_t(flx_thread_wrapper_t const&); // uncopyable
  void operator=(flx_thread_wrapper_t const&); // uncopyable
public:
  ~flx_thread_wrapper_t();
  flx_thread_wrapper_t(void (*start)(void*), void* udat, thread_control_t *tc);
};

}}
#endif


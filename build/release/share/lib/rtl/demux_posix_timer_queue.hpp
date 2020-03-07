#ifndef __FLX_DEMUX_POSIX_TIMER_QUEUE_H__
#define __FLX_DEMUX_POSIX_TIMER_QUEUE_H__

#include "pthread_thread.hpp"  // flx_thread_t
#include "demux_timer_queue.hpp" // base class
#include <sys/time.h>        // timespecs, gettimeofday
#include <thread>
#include <condition_variable>

namespace flx { namespace demux {

// looks like a worker queue, but couldn't quite mash it into one
class DEMUX_EXTERN posix_timer_queue : public timer_queue
{
    ::std::mutex lock; // factor to prio queue?
    ::std::condition_variable_any sleep_cond;
    flx::pthread::flx_thread_t sleep_thread; // joinable, we join later
    void*        opaque_prio_queue;        // less fat

    static void thread_start(void*);    // passed "this"
    bool thread_loop_body();


    void wakeup_thread();                // we can do this!

    void add_sleep_request(sleep_task* st, timespec* abs);
public:
    posix_timer_queue();
    ~posix_timer_queue();

    // thread safe.
    virtual void add_sleep_request(sleep_task* st, double delta);

    // in seconds, relative to same base as timer::get_time.
    virtual void add_abs_sleep_request(sleep_task* st, double when);
};

}}

#endif // __POSIX_TIMER_QUEUE__

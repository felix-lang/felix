#include "demux_posix_timer_queue.hpp"

// a prio queue that executes tasks in a given order
// factor out prio_queue? could be like queue.

// try to make work like the worker thread thing, fix it do so?.
// remove time from sleep task...

#include <iostream>   // for cerr
#include <queue>      // stl seems to have a prio_queue
#include <stdio.h>    // for perror
#include <sys/time.h> // gettimeofday for calculating "now"
#include <chrono>

//using namespace flx::pthread;
namespace flx { namespace demux {

timer_queue *mk_timer_queue() { return new posix_timer_queue; }


#define MIL 1000000        // one million
#define BIL (MIL*1000)    // one billion (metric)

using namespace std;

// it could happen!
// factor
class future_evt
{
public:
    timespec    when;
    sleep_task*    task;

    // ignore the direction, just trying to sort with smallest first
    bool operator<(const future_evt& rhs) const
    {
        if(when.tv_sec != rhs.when.tv_sec)    // precedence to more significant
            return when.tv_sec > rhs.when.tv_sec;
        else                                // else check the less significant
            return when.tv_nsec > rhs.when.tv_nsec;
    }
};

typedef priority_queue<future_evt> void_prio_queue;
#define PRIOQ ((void_prio_queue*)opaque_prio_queue)

posix_timer_queue::posix_timer_queue()
{
    opaque_prio_queue = new void_prio_queue;    // a.k.a. PRIOQ
    // cerr << "initing timer sleep thread" << endl;

    // NEED'S TO CHECK RETURN VAL AND HANDLE ERROR
    if(sleep_thread.init(thread_start, this, NULL))
        cerr << "failed to create posix timer queue thread!" << endl;
}

posix_timer_queue::~posix_timer_queue()
{
    // the sleep_thread uses the prioq, so we must explicitly shut it
    // down now, before we delete the prioq. left to its own devices,
    // c++ destructs it at the end of this destructor.

    // take down the thread first because it uses all the other stuff.
    // I actually don't need to do anything special to bring the thread
    // down because all pthread_cond_*wait* are cancel aware. Or so they
    // should be. As far as I can tell only the 64bit osx10.4.2 is, so
    // for now the explicit cancel + wakeup followed by explicit
    // cancel test stays.

    // cerr << "asking timer thread to quit" << endl;
    add_sleep_request(NULL, 0.0);    // super secret quit thread quit request
    wakeup_thread();                // wakeup, cause to goto a cancel pt

    sleep_thread.join();            // will join
    // cerr << "about to delete PRIOQ" << endl;
    delete PRIOQ;
}

static void
get_now(timespec* now)
{
    struct timeval tp;

    if(gettimeofday(&tp, NULL) == -1)
        perror("gettimeofday");

    // (10^6-1)*1000 = 3B9AC618 = max usec -> nsec fits in a 32bit long.
    now->tv_sec = tp.tv_sec;
    now->tv_nsec = tp.tv_usec*1000;        // fits!

    // cerr << "get_now = " << now->tv_sec << ", " << now->tv_nsec << endl;
}

// LIMIT!
// seconds to microseconds - signed this gives a bit over half an hour
#define SEC2TIMESPEC(ts, t) long    wait_musec = (long)(t*MIL);    \
    timespec    ts = { wait_musec / MIL, (wait_musec % MIL)*1000 }


// offset delta from "now" and store in "when"
static void
calc_when(timespec* when, double delta)
{
// how to use the posix abstime versions of timed waits? what kind of absolute
// is abstime? pthread_get_expiration_np looks useful, but it too is np.
// abstime is apparently in seconds since the Epoch, UTC.
// To get now there's clock_gettime (not portable) or gettimeofday with
// null timezone.

    timespec    now;
    get_now(&now);

    // limit!
    // seconds to microseconds - signed this gives a bit over half an hour
    // long    wait_musec = (long)(delta*MIL);
    // timespec    delay = { wait_musec / MIL, (wait_musec % MIL)*1000 };
    SEC2TIMESPEC(delay, delta);

    // (10^6-1)*1000 = 3B9AC618 = max usec -> nsec fits in a 32bit long.
    when->tv_sec = now.tv_sec + delay.tv_sec;
    when->tv_nsec = now.tv_nsec + delay.tv_nsec;

    if(when->tv_nsec >= BIL)            // overflow of nanoseconds?
    {
        // cerr << "OVERFLOW = " << when->tv_sec << ", " << when->tv_nsec << endl;
        // x, y < BIL, x + y < 2BIL
        when->tv_sec++;
        when->tv_nsec -= BIL;
        // when->tv_sec += when->tv_nsec/BIL;
        // when->tv_nsec %= BIL;
    }

    // cerr << "when = " << when->tv_sec << ", " << when->tv_nsec << endl;
    // tp contains tv_sec (seconds) & tv_usec (microseconds) both longs.
    // however, if nonposix works everywhere...
}

// absolute time
void
posix_timer_queue::add_sleep_request(sleep_task* st, timespec* abs)
{
    future_evt    evt;
    evt.task = st;
    evt.when = *abs;

    ::std::unique_lock< ::std::mutex> locker(lock);

    PRIOQ->push(evt);

    // we may have inserted at sooner than any other evt, so wake up thread
    // to figure it out (if need be). I seemed to be getting more wakeups
    // with this. Turned off for now. Not sure how that works.
    if(1 || PRIOQ->top().task == st)
    {
      // cerr << "WE PUSHED IN - waking thread" << endl;
      wakeup_thread();
    }
}

// note: may not need time to be in sleep_task. could pass time here.
// thread safe
void
posix_timer_queue::add_sleep_request(sleep_task* st, double delta)
{
    // cerr << "add_sleep_request: " << delta << endl;
    timespec    when;
    calc_when(&when, delta);        // calculate when (t a delta)

    add_sleep_request(st, &when);
}

void
posix_timer_queue::add_abs_sleep_request(sleep_task* st, double when)
{
    // absolute version is closer to the posix implementation
    SEC2TIMESPEC(abs_time, when);
    add_sleep_request(st, &abs_time);
}

// cause the timer wait thread to wake up. useful for asking it to
// exit or re-evaluate a changed sleep queue.
void
posix_timer_queue::wakeup_thread()
{
    sleep_cond.notify_all();
}

void
posix_timer_queue::thread_start(void* udat)
{
    posix_timer_queue*    q = (posix_timer_queue*)udat;
    //cerr << "sleeper thread" << endl;

    while(q->thread_loop_body()) ;
}

bool
posix_timer_queue::thread_loop_body()
{
    // lock on. lock off when waiting on condition
    ::std::unique_lock< ::std::mutex> locker(lock);

    // int        res;

    // pthread_cond_wait & pthread_cond_timedwait (& np rel version?) are
    // cancellation points. doco notes for timed & untimed waits that the
    // predicate should be rechecked as there can be spurious wakeups.
    // no worries, when we wakeup the lock has been acquired.

    while(!PRIOQ->empty())
    {
        future_evt    evt = PRIOQ->top();

        // quit request
        if(!evt.task) return false;

        future_evt  now;        // "now' has no task, just a dummy.
        get_now(&now.when);

        // if(evt < now)        // would prefer <=, eh.
        // < is arse backwards because I don't know how to use the stl
        if(now < evt)        // would prefer <=, eh.
        {
            // cerr << "firing of (" <<
            //    evt.when.tv_sec << ", " << evt.when.tv_nsec << ") at (" <<
            //    now.when.tv_sec << ", " << now.when.tv_nsec << ") " << endl;
            evt.task->fire();
            PRIOQ->pop();
        }
        else    // we have an event in future, so sleep for that long
        {
            // remember that condition waits are exit points...
            // so I don't need to test - check that.
            // cerr << "sleeping from (" <<
            //    evt.when.tv_sec << ", " << evt.when.tv_nsec << ") until (" <<
            //    now.when.tv_sec << ", " << now.when.tv_nsec << ") " << endl;
            
            auto x = 
               ::std::chrono::seconds{evt.when.tv_sec} + 
               ::std::chrono::nanoseconds{evt.when.tv_nsec}
            ;
            auto d = ::std::chrono::duration_cast<::std::chrono::system_clock::duration> (x);
            ::std::chrono::system_clock::time_point tp (d);
            (void)sleep_cond.wait_until(lock, tp);

            // if using posix abstime timed wait we make get EINVAL here for
            // abstimes in the past. must handle this.
            //JS: It's handled now, waiting for a time in the past is OK

            // cerr << "pthread_cond_timedwait woke up! (" << res << ")" << endl;
        }
    }

    // if we got here then the queue is empty, so sleep indefinitely
    // that we don't really need the mainloop testcancel because the condition
    // wait functions are cancellation points.
    // cerr << "no sleep task, sleeping indefinitely" << endl;
    sleep_cond.wait(lock);
    // cerr << "pthread_cond_wait woke up! (", res << ")" << endl;

    // lock released here
    return true;                    // keep going
}


// in seconds from some ref pt
// N.B. declared in base class!
void
timer_queue::get_time(double& t)
{
    timespec    now;
    get_now(&now);        // just calls gettimeofday (msec) and converts
                        // to timespec (sec, nsec). could skip that
                        // and call directly, avoiding conversion
    t = now.tv_sec + (now.tv_nsec*BIL);
}

}}

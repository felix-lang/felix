#include "flx_demux_config.hpp"
#include <Windows.h>
#include <assert.h>

// simply wrapped windows timer queue. requires windows 5.00, which is
// quite high (xp?) because I couldn't get the waitable timers to work.
// must be careful with this stuff lest it create millions of threads.
#include "demux_win_timer_queue.hpp"

#include <stdio.h>

namespace flx { namespace demux {

timer_queue *mk_timer_queue() { return new win_timer_queue; }

#define MIL 1000000    // 1 metric million

typedef struct
{
  sleep_task*  st;        // so we can make it fire
  HANDLE    timer;      // we need to delete the timer, so we keep it
  HANDLE    timer_queue;  // AND its queue (no back ptrs, I guess)
} timer_cookie;

win_timer_queue::win_timer_queue()
{
  // fprintf(stderr,"win_timer_queue ctor\n");

  timer_queue = CreateTimerQueue();
  if(!timer_queue)
  {
    fprintf(stderr, "CreateTimerQueue failed: %i\n", GetLastError());
    throw -1;
  }
  // fprintf(stderr, "created timer queue: %p\n", timer_queue);
}


win_timer_queue::~win_timer_queue()
{
  // INVALID_HANDLE_VALUE indicates that DeleteTimerQueueEx should wait for
  // all callback functions to complete before returning. One would hope that
  // calling this causes all the timers to go off before their time (what
  // else would the "actually fired" callback flag be for?). The alternative
  // of waiting for some ever distant timer to go off would be too stupid
  // for words. As usual, the msdn glosses over the important details like
  // this one. Anyway, it's easy to test out... No, that flag's always true
  // for timers, and this wait option doesn't work - maybe with other types
  // flags for CreateTimerQueueTimer?
  if(!DeleteTimerQueueEx(timer_queue, INVALID_HANDLE_VALUE))
  {
    fprintf(stderr, "DeleteTimerQueueEx failed: %i\n", GetLastError());
    // whatcha gonna do about it?
  }
  // fprintf(stderr, "finished - did it wait?\n");
}

// note: may not need time to be in sleep_task. could pass time here.
// thread safe
void
win_timer_queue::add_sleep_request(sleep_task* st, double delta)
{
  // fprintf(stderr,"add_sleep_request: %lf to %p\n", delta, timer_queue);

  timer_cookie*  tc = new timer_cookie;

  // copy in the sleep_task and the timer queue
  tc->st = st;
  tc->timer_queue = timer_queue;

  // the timer thread may not be the best solution as nothing is stopping
  // anyone from performing long operations with this structure, however
  // in all likelihood, it'll just be felix adding threads back to its queue.
  if(!CreateTimerQueueTimer(
    &tc->timer,          // resulting timer in timer_cookie
    timer_queue,
    //NULL,            // add to default timer queue
    timer_callback,        // should get called in delta seconds
    tc,             // timer cookie is user data
    (DWORD)(delta*1000),    // millisecond timer
    0,              // zero period => signal once
    WT_EXECUTEINTIMERTHREAD))  // NB: for short tasks (will this do?)
  {
    fprintf(stderr, "CreateTimerQueueTimer failed: %i\n", GetLastError());
    delete tc;          // at least try not to leak
    return;
  }
}

// this is a c callback - all the c++ code should probably be wrapped
// in a try/catch. timer_or_wait_fired is always true for timers.
VOID CALLBACK
win_timer_queue::timer_callback(PVOID udat, BOOLEAN timer_or_wait_fired)
{
  timer_cookie*  tc = (timer_cookie*)udat;

  // fprintf(stderr, "timer queue callback fired: %p, %i\n",
  //  tc, timer_or_wait_fired);

  if(!tc)
  {
    // Nothing that we can do in this situation.
    fprintf(stderr, "WHOA - NULL queue cookie! (fired: %i)\n",
      timer_or_wait_fired);
    return;            // outta here
  }

  // NULL means delete the thing now, INVALID_HANDLE_VALUE means wait until
  // callback finishes. We're in the callback, so we can't do that (=deadlock
  // of the timer thread, which isn't good). We're all adults here, the timer
  // has expired, we know what we're doing, so lets just delete it.
  tc->st->fire();

  // on my box this returns ERROR_IO_PENDING, on others it doesn't
  // msdn says this should be ok, but I'm not so sure.
  if(!DeleteTimerQueueTimer(tc->timer_queue, tc->timer, NULL))
  {
    int  err = GetLastError();

    if( ERROR_IO_PENDING != err)
    {
      fprintf(stderr, "DeleteTimerQueueTimer of %p failed: %i\n",
        tc->timer, err);
    }
    else
    {
      // I'm not so sure, see if it leaks.
      //fprintf(stderr, "DeleteTimerQueueTimer = ERROR_IO_PENDING\n");
      //fprintf(stderr, "Apparently this is ok...\n");
    }
  }
  delete tc;

  // fprintf(stderr, "leaving timer callback\n");
}

// in seconds from some ref pt (UTC for this fn)
// N.B. declared in base class!
void
timer_queue::get_time(double& t)
{
  SYSTEMTIME  sysnow;
  GetSystemTime(&sysnow);
  // now convert to seconds
  // via FILETIME?

  // kinda sucks, but is the msdn recommended way of doing calculations
  // on dates.
  FILETIME  fnow;
  if(!SystemTimeToFileTime(&sysnow, &fnow))
  {
    fprintf(stderr, "SystemTimeToFileTime failed: %i\n", GetLastError());
    t = 0;
    return;
  }

  ULARGE_INTEGER now;  // so we can do some maths

  assert(sizeof(now) == sizeof(fnow));
  memcpy(&now, &fnow, sizeof(now));

  // and now we have a big integer containing an offset jan 1, 1601 (UTC)
  // 100 nanosecond intervals
  t = now.QuadPart*MIL*10;  // *10 to microseconds, *MIL to seconds
}

void
win_timer_queue::add_abs_sleep_request(sleep_task* st, double when)
{
  // win timer queue works with relative offsets, so convert this absolute
  double  now;
  get_time(now);
  double  delta = when-now;
  if(delta < 0.0) delta = 0.0;
  add_sleep_request(st, delta);
}

}}

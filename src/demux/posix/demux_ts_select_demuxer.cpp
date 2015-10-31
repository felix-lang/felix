#include "demux_ts_select_demuxer.hpp"

// #include <stdio.h>

namespace flx { namespace demux {

ts_select_demuxer::ts_select_demuxer()
{
  // fprintf(stderr, "creating pipe for self-pipe trick\n");
  // install self pipe trick
  sp.install(&demux);
}

ts_select_demuxer::~ts_select_demuxer()
{
  async_quit(); // wake thread, ask to leave.
}

void
ts_select_demuxer::get_evts(bool poll)
{
  fd_set  rset, wset, exset;

  // copy args under lock
  {
    ::std::unique_lock< ::std::mutex> locker(ham_fist);
    demux.copy_sets(rset, wset, exset);
  }

  // process arg set under lock. note that the select demuxer is passed
  // to wakeups, so no recursive lock is needed. also, because any
  // readditions caused by the callback are done to the naive demuxer,
  // no selfpipe writes are required either. the only thing to remember
  // is that the wakeup recipient should not be surprised to see a demuxer
  // in its callback different to the one it originally added to.
  if(demux.select(rset, wset, exset, poll))
  {
    ::std::unique_lock< ::std::mutex> locker(ham_fist);
    demux.process_sets(rset, wset, exset);
  }
}

// thread safe overloaded functions follow. all acquire the lock
// then call the unthreadsafe version of the function. nice!
int
ts_select_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  // fprintf(stderr, "ts_select::add: %p->%i (%s), %x\n",
  //  sv, s, (s == self_pipe_fds[0]) ? "self pipe" : "socket", flags);
  ::std::unique_lock< ::std::mutex> locker(ham_fist);

  int res = demux.add_socket_wakeup(sv, flags);
  // I wouldn't touch the sv after this.

  // we have a new socket, so wake the event waiting thread
  // for great responsiveness.
  if(-1 != res) sp.wake();

  return res;

// we need to wake a blocking get_evts call here else we'll have bad
// performance or even a lockup. the question is do we need to do it under
// the tutelage of the lock?
}

}}

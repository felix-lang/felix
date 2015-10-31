#include "demux_ts_poll_demuxer.hpp"
#include <stdio.h>

namespace flx { namespace demux {

ts_poll_demuxer::ts_poll_demuxer()
{
  //fprintf(stderr, "ts_poll_demuxer installing self-piper\n");
  // install self pipe trick.
  sp.install(&demux);
}

ts_poll_demuxer::~ts_poll_demuxer()
{
  //fprintf(stderr, "ts_polling asking thread to quit\n");
  async_quit();  // get async waiting thread to stop waiting
  //fprintf(stderr, "ts_poll async quit finished\n");
}

void
ts_poll_demuxer::get_evts(bool poll)
{
  void  *fds, *svs;

  // copy args under lock
  {
    ::std::unique_lock< ::std::mutex> locker(ham_fist);
    demux.get_arrays(&fds, &svs);  // the arrays are now mine
    // lock released
  }

  // do the poll
  int  nevts = demux.dopoll(fds, poll);

  // regardless of the number of events, I have to copy the pieces back
  // under lock tutelage
  {
    ::std::unique_lock< ::std::mutex> locker(ham_fist);
    demux.process_evts(fds, svs, nevts);
    // lock released
  }
}

// thread safe overloaded functions follow. all acquire the lock
// then call the unthreadsafe version of the function. nice!
int
ts_poll_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  // fprintf(stderr, "ts_poll::add_sock(%i, %x)\n", sv->s, flags);
  ::std::unique_lock< ::std::mutex> locker(ham_fist);

  int  res = demux.add_socket_wakeup(sv, flags);
  // I wouldn't touch the sv after this.

  if(-1 != res) sp.wake();

  return res;
}
}}

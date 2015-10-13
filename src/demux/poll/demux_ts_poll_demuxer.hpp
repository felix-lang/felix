#ifndef __FLX_DEMUX_TS_POLL_DEMUXER_H__
#define __FLX_DEMUX_TS_POLL_DEMUXER_H__

// thread safe version of poll_demuxer

#include "demux_poll_demuxer.hpp"
#include "demux_self_piper.hpp"     // self pipe trick
#include <thread>
#include <mutex>

namespace flx { namespace demux {

class ts_poll_demuxer : public posix_demuxer {
  // lock
  ::std::mutex ham_fist;
  // protects this little fella here.
  poll_demuxer    demux;

  self_piper      sp;
protected:
  virtual void    get_evts(bool poll);
public:
  ts_poll_demuxer();
  ~ts_poll_demuxer();

  virtual int     add_socket_wakeup(socket_wakeup* sv, int flags);

  // pass it on to composed non-threadsafe demuxer
  virtual demux_quit_flag* get_quit_flag() { return demux.get_quit_flag(); }
  virtual void set_quit_flag(demux_quit_flag* f) { demux.set_quit_flag(f); }
};

}} // namespace demux, flx

#endif

#ifndef __FLX_DEMUX_TS_SELECT_DEMUXER_H__
#define __FLX_DEMUX_TS_SELECT_DEMUXER_H__

#include "demux_select_demuxer.hpp"
#include "demux_self_piper.hpp"
#include <thread>
#include <mutex>

namespace flx { namespace demux {

// thread safe version of select demuxer

class DEMUX_EXTERN ts_select_demuxer : public posix_demuxer {
  // lock
  ::std::mutex ham_fist;
  // protects this little fella here.
  select_demuxer  demux;

  // self pipe trick for waking waiting thread when we like.
  // for demuxer responsiveness.
  self_piper sp;
protected:
  virtual void    get_evts(bool poll);
public:
  ts_select_demuxer();
  ~ts_select_demuxer();

  virtual int   add_socket_wakeup(socket_wakeup* sv, int flags);

  // pass it on to composed non-threadsafe demuxer
  virtual demux_quit_flag* get_quit_flag() { return demux.get_quit_flag(); }
  virtual void set_quit_flag(demux_quit_flag* f) { demux.set_quit_flag(f); }
};
}} // namespace demux, flx

#endif

#ifndef __FLX_DEMUX_EPOLL_DEMUXER_H__
#define __FLX_DEMUX_EPOLL_DEMUXER_H__

#include <flx_demux_config.hpp>
#include "demux_posix_demuxer.hpp"

namespace flx { namespace demux {
// epoll allows only one event per socket - it does not differentiate
// on the awaited operation (read/write), however it does let you wait
// on any combination (I think)

// ********************************************************
/// epoll based demuxer
// ********************************************************

class DEMUX_EXTERN epoll_demuxer : public posix_demuxer {
  int   epoll_fd;

  // be careful of this - don't let it create race conditions
  // should probably only be called by wait = in one thread only (check)
  // this removes ALL outstanding events for s.
  void  remove_wakeup(int s);

  virtual void  get_evts(bool poll);
public:
  epoll_demuxer();
  virtual ~epoll_demuxer();

  virtual int   add_socket_wakeup(socket_wakeup* sv, int flags);
};

}} // namespace demux, flx
#endif

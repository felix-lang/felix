#ifndef __FLX_DEMUX_KQUEUE_DEMUXER_H__
#define __FLX_DEMUX_KQUEUE_DEMUXER_H__

#include "demux_posix_demuxer.hpp"

namespace flx { namespace demux {

// ********************************************************
/// kqueue demuxer for osx'n'BSD and at least 1 linux
// ********************************************************
class DEMUX_EXTERN kqueue_demuxer : public posix_demuxer {
  int   kq;
protected:
  // this could just be passed the socket_wakeup, if it stored
  // the flags. Those flags are also set, though, which would
  // create a race condition. In and out flags?

  int add_kqueue_filter(socket_wakeup* sv, short filter);
  int remove_kqueue_filter(int s, short filter);

  int remove_socket_wakeup(int s, int flags);
  void get_evts(bool poll);
public:
  kqueue_demuxer();
  virtual ~kqueue_demuxer();

  virtual int add_socket_wakeup(socket_wakeup* sv, int flags);
};

}} // namespace demux, flx
#endif

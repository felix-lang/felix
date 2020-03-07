#ifndef __FLX_DEMUX_QUITTER_H__
#define __FLX_DEMUX_QUITTER_H__

#include <flx_demux_config.hpp>
#include "demux_demuxer.hpp"  // demuxers

#if FLX_WIN32
#include "demux_wself_piper.hpp" // win32 self piper
#else
#include "demux_self_piper.hpp" // posix self piper
#endif

#include "pthread_waitable_bool.hpp"

namespace flx { namespace demux {

// quits a demuxer
class DEMUX_EXTERN demux_quitter
        : public demux_callback, public demux_quit_flag {
  // self pipes for getting demuxer attention
#if FLX_WIN32
  wself_piper sp;
#else
  self_piper sp;
#endif
  pthread::waitable_bool finished;  // initially false
  void callback(demuxer* demux); // called back by demuxer in event thread.
  virtual void signal_true(); // signal finish, from demux_quit_flag
public:
  void quit(demuxer* demux); // blocks until event thread exits
};

}}

#endif

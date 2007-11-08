#ifndef __FLX_DEMUX_SELF_PIPER_H__
#define __FLX_DEMUX_SELF_PIPER_H__

#include <flx_demux_config.hpp>
#include "demux_posix_demuxer.hpp"

namespace flx { namespace demux {

// there's no standard posix_socketio_wakeup, could be handy. could also
// perhaps use it here? this is a pipe, not a socket. not sure if recv nor
// send work on it, besides want to read an unlimited amount of redundant data.
class DEMUX_EXTERN selfpipe_wakeup : public socket_wakeup {
public:
  demux_callback* cb; // optional callback

  virtual void wakeup(posix_demuxer& demux);
};

class DEMUX_EXTERN auto_fd {
public:
    int fd;

    auto_fd();
    ~auto_fd();
};

// make portable here? make part of the wakeup obj?
class DEMUX_EXTERN pipe_pair {
  // self pipe trick!!! fd[0] = read end, fd[1] = write end.
  auto_fd         fds[2];
public:
  pipe_pair();
  // void read_byte(); // done for us by wakeup obj.
  void write_byte();
  int get_read_end();
};

// wakes a POSIX demuxer, for when you want some kind of attention
// todo: make portable
class DEMUX_EXTERN self_piper {
    pipe_pair       pp;
    selfpipe_wakeup spw;
public:
    void install(demuxer* demux, demux_callback* cb = 0);
    void wake();
};

}} // namespace demux, flx

#endif

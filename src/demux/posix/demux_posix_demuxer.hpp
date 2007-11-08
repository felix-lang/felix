#ifndef __FLX_DEMUX_POSIX_DEMUXER_H__
#define __FLX_DEMUX_POSIX_DEMUXER_H__

// base classes for posix style demuxers

#include "demux_demuxer.hpp"

namespace flx { namespace demux {
class DEMUX_EXTERN posix_demuxer;            // fwd decl

// abc
class DEMUX_EXTERN posix_wakeup {
public:
  virtual ~posix_wakeup() {}

  // when called, the wakeup has finished and been removed.
  virtual void wakeup(posix_demuxer& demux) = 0;
};

class DEMUX_EXTERN socket_wakeup : public posix_wakeup {
public:
  int   s;                // the non blocking socket
  int   wakeup_flags;         // set on wakeup, r/w or both

  socket_wakeup() : s(-1) {}
};

class DEMUX_EXTERN posix_demuxer : public demuxer {
protected:
  void async_quit(); // useful for requesting wait thread quit in
                     // thread safe demuxer destructors. doesn't throw.

public:
  virtual ~posix_demuxer();

  // posix style sockets. for reading and writing (but not both at once
  // for the same socket_wakeup) you are guaranteed to receive only one
  // wakeup per call to this function when you call wait.
  // returns -1 if no wakeup is coming and zero if one is.
  // For simultaneous reading and writing you may get two wakeups,
  // that is, it may violate the "one shot" rule. Ignoring for now,
  // as it's not a common use. This makes it undefined behaviour.
  // wakeup is owned by the demuxer until its wakeup is called,
  // so treat it with kid gloves, i.e. don't mess with it.
  virtual int   add_socket_wakeup(socket_wakeup* sv, int flags) = 0;

  // to be called when we can read & write without blocking
  // return true if connection closed, update pb
  // sort of a strange place to have this..., more a socket wakeup
  // thing, even if static
  static bool   socket_recv(int s, sel_param* pb);
  static bool   socket_send(int s, sel_param* pb);
};

// some handy control blocks for common non-blocking socket operations
// note that they "fortuitously" both have start methods. hmm.
// a socket io one could be handy here.

// this one's restartable (makes sense for listener sockets)
class DEMUX_EXTERN accept_control_block : public socket_wakeup {
public:
  int   accepted;   // accepted socket (out)
  int   socket_err;   // the error, if acceptee == -1, else 0 (out)

  accept_control_block() : accepted(-1), socket_err(0) {}

  virtual int start(posix_demuxer& demux);
  virtual void wakeup(posix_demuxer& demux);
};

class DEMUX_EXTERN connect_control_block : public socket_wakeup {
public:
  int     socket_err;   // outgoing error (on start or wake)
  // this should probably be a sockaddr type
  const char* addy;     // addr (dotted quad) (in)
  int     p;        // port (in)

  connect_control_block() : socket_err(0) {}

  virtual int start(posix_demuxer& demux);
  virtual void wakeup(posix_demuxer& demux);

  // oops, can't check for s != -1 as it's always there.
  // was always "finished" and so I started io, losing the first wakeup
  // on epoll evtsrc. Is this right, or should it be != EINPROGRESS?
  // keep in sync with iocp version. give socket_err initial definition
  // that works with this?
  bool finished() { return ( 0 == socket_err); }
};

}} // namespace demux, flx
#endif

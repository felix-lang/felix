#ifndef __FLX_FAIO_POSIXIO_H__
#define __FLX_FAIO_POSIXIO_H__
#include <flx_faio_config.hpp>

#include "faio_asyncio.hpp"

// we don't need to piggyback much data at all. for now just the demuxer,
// so that we can be woken up, and the buffer info (this replaces the
// felix "socket" thread type, which was ugly.

#include "demux_posix_demuxer.hpp"

// a new sort of demuxer/event source: file io completions
// haven't given up on using the socket style demuxers yet.
#include "demux_pfileio.hpp"

#include "demux_timer_queue.hpp"

namespace flx { namespace faio {

class FAIO_EXTERN socketio_wakeup : public demux::socket_wakeup {
public:
  demux::sel_param   pb;     // in: what you want, out: what you get
  int       sio_flags;  // either one of PDEMUX_{READ|WRITE}A
  class socketio_request *request;

  virtual void wakeup(demux::posix_demuxer& demux);
};

// this can handle most unix style io, that is, read & write on sockets,
// files & pipes. NICE. the fact that the socket is now in here may mean
// I can get rid of the epoll hack
// Not sure if this can be used for file fds.
class FAIO_EXTERN socketio_request : public flx_driver_request_base {
public:
    socketio_wakeup sv;
    demux::posix_demuxer *pd;
    socketio_request() {}       // Lord Felix demands it. Like STL.
    socketio_request(socketio_request const&);
    void operator = (socketio_request const&);

    socketio_request(demux::posix_demuxer *pd_a, int s, char* buf, long len, bool r);
    bool start_async_op_impl();
};

// client open
class FAIO_EXTERN connect_request
  : public flx_driver_request_base, public demux::connect_control_block {
public:
  demux::posix_demuxer *pd;
  connect_request() {}      // flx linkage

  connect_request(demux::posix_demuxer *pd_a,const char* addr, int port);
  bool start_async_op_impl();
  virtual void wakeup(demux::posix_demuxer&);
};

// server open
class FAIO_EXTERN accept_request
  : public flx_driver_request_base, public demux::accept_control_block {
public:
  // we sometimes know that there'll be several connections to accept.
  // this'll need a different wakeup - and a different interface between
  // event source & wakeups

  demux::posix_demuxer *pd;
  accept_request() {} // flx linkage

  // eeh, give that a better name
  accept_request(demux::posix_demuxer *pd_a, int listener) : pd(pd_a) { s = listener; }

  // from flx_driver_request_base
  bool start_async_op_impl();

  // from accept_control_block
  virtual void wakeup(demux::posix_demuxer& demux);
};


// separate pthread file io
// hum. multiple inheritance
class FAIO_EXTERN flxfileio_request
    : public flx_driver_request_base, public demux::fileio_request
{
    pthread::worker_fifo       *aio_worker;
public:
    flxfileio_request();           // flx linkage
    ~flxfileio_request();          // flx linkage

    flxfileio_request(
      pthread::worker_fifo *a,
      int f, char* buf, long len, long off, bool rd
     )
        : fileio_request(f, buf, len, off, rd), aio_worker(a)
     {
       //fprintf(stderr, "flxfilio_request ctor\n");
     }

    // from driver request
    bool start_async_op_impl();
    void finished(); // fileio_request
};

}}
#endif

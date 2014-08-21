#include <stdio.h>      // printf
#include "faio_posixio.hpp"
#include "demux_sockety.hpp"    // async_connect

#include <sys/types.h>  // getsockopt & co
#include <sys/socket.h>

#include <unistd.h>     // close
#include <string.h>     // strerror - probably not portable
#include <assert.h>

using namespace flx::demux;
namespace flx { namespace faio {

connect_request::connect_request(demux::posix_demuxer *pd_a,const char* addr, int port) :pd(pd_a) { addy = addr; p = port; s=-1; }

socketio_request::socketio_request(demux::posix_demuxer *pd_a, int s, char* buf, long len, bool read)
: pd(pd_a)
{
  //fprintf(stderr,"socketio_request %p making socketio_wakeup for socket %d\n",this,s);
  sv.s = s;
  sv.request = this;
  // demux supports reading AND writing. We don't. Yet.
  sv.sio_flags = ((read) ? PDEMUX_READ : PDEMUX_WRITE);

  sv.pb.buffer = buf;
  sv.pb.buffer_size = len;
  sv.pb.bytes_written = 0;        // really bytes_processed
}

socketio_request::socketio_request(socketio_request const &a) : pd(a.pd)
{
  //fprintf(stderr, "copying socketio_request to %p\n",this);
  sv = a.sv;
  sv.request = this;
}

// EXTREME HACKERY!
void socketio_request::operator=(socketio_request const &a)
{
  //fprintf(stderr, "assigning socketio_request to %p\n",this);

  flx_driver_request_base::operator=(a);
  sv = a.sv;
  sv.request = this;
  pd = a.pd;
}

bool
socketio_request::start_async_op_impl()
{
  //fprintf(stderr,"socketio_request: socket %d start async_op_impl %p\n",sv.s,this);
  // fprintf(stderr, "adding wakeup: len %i, done %i\n",
  //   sv.pb.buffer_size, sv.pb.bytes_written);

  if(sv.s == -1) {
    fprintf(stderr, "Attempt to start_async_op on socket -1\n");
    exit(1);
  }

  // wake thread if call failed
  bool failed = (pd->add_socket_wakeup(&sv, sv.sio_flags) == -1);
  if (failed)
    fprintf(stderr,"socketio_request FAILED %p, sock=%d, dir=%d\n",this, sv.s, sv.sio_flags);
  //else
  //  fprintf(stderr,"socketio_request OK %p\n",this);
  return failed;
}


void
socketio_wakeup::wakeup(posix_demuxer& demux)
{
  //fprintf(stderr, "Wakeup, socket = %d\n",s); 
  // handle read/write, return true if not finished.
  // otherwise wakeup return false.
  bool  connection_closed;

  //fprintf(stderr, "making socketio_wakeup %p\n",this);
  //fprintf(stderr,"prehandle wakeup, this: %p, read: %i, len: %i, done %i\n",
  //  this, read, pb.buffer_size, pb.bytes_written);

  // NOTE: this code does not handle the possibility of both read AND
  // write being set. That would require thinking about the what
  // the connect_closed return value meant. In any case, we don't
  // do that stuff here yet.

  if(wakeup_flags & PDEMUX_ERROR)
  {
    connection_closed = true;
    //pb.bytes_written=0;
    fprintf(stderr,"posix faio wakeup PDEMUX_ERROR, connection closed = %d\n", connection_closed);
  }

  else if(wakeup_flags & PDEMUX_EOF)
  {
    connection_closed = true;
    fprintf(stderr,"posix faio wakeup PDEMUX_EOF, connection closed = %d\n", connection_closed);
    //pb.bytes_written=0;
  }

  else if(wakeup_flags & PDEMUX_READ)
  {
    // just check that our above assumption hasn't been violated.
    assert(wakeup_flags == PDEMUX_READ);
    //fprintf(stderr,"posix faio wakeup PDEMUX_READ, reading..\n");
    connection_closed = posix_demuxer::socket_recv(s, &pb);
    //fprintf(stderr,"posix faio wakeup PDEMUX_READ, connection closed = %d\n", connection_closed);
  }
  else
  {
    // never hurts to be paranoid.
    assert(wakeup_flags == PDEMUX_WRITE);
    //fprintf(stderr,"posix faio wakeup PDEMUX_WRITE, writing..\n");
    connection_closed = posix_demuxer::socket_send(s, &pb);
    //if(connection_closed)
    //  fprintf(stderr,"posix faio wakeup PDEMUX_WRITE, connection closed = %d\n", connection_closed);
  }

  // fprintf(stderr,"posthandle wakeup, this: %p, read: %i, len: %i, done %i\n",
  //  this, read, pb.buffer_size, pb.bytes_written);
  // fprintf(stderr,"wakeup of %p, closed = %i\n", this, connection_closed);

  // wake up: time to process some data
  if(connection_closed || pb.bytes_written == pb.buffer_size)
  {
    // fprintf(stderr,"schedding %p, drv: %p, f: %p\n", this, drv, f);
    // if the connection closed, this notify should tell the caller
    // not to keep trying to write, but it doesn't .. why not?
    // who called it anyhow?
    // I think the writing code ignores error returns ..
    request->notify_finished();
    return;
  }

  // fprintf(stderr,"not schedding %p\n", this);
  fprintf(stderr, "Incomplete request on %d, waiting for more I/O\n",s);
  if(demux.add_socket_wakeup(this, sio_flags) == -1)
  fprintf(stderr,"failed to re-add_socket_wakeup\n");
}

// asynchronous connect
bool
connect_request::start_async_op_impl()
{
  //fprintf(stderr,"connect_request %p: start async_op_impl\n",this);

  // call failed or finished (!), wake up thread as no wakeup coming
  if(start(*pd) == -1) {
    fprintf(stderr, "FAILED TO SPAWN CONNECT REQUEST\n");
    return true;
  }

  // NONONONONO! Referring to this's variables after a successful start
  // gives rise to a race condition, which is bad.
  //fprintf(stderr, "CONNECT REQUEST SPAWNED\n");
  return false;     // do not reschedule after a successful start

/*
  // I've not seen this yet, don't know why.
  if(0 == socket_err) fprintf(stderr, "WOW, instant CONNECT\n");

  // call didn't fail, could be pending or finished.
  // return socket_err != EINPROGRESS, the contrapositive, sort of
  return 0 == socket_err;   // no err => finished immediately
*/
}

void
connect_request::wakeup(posix_demuxer& demux)
{
  //fprintf(stderr, "connect_request::wakeup\n");

  // fprintf(stderr,"connect woke up\n");
  connect_control_block::wakeup(demux);

  // felix thread can pick out error itself.
  notify_finished();
}


// async accept
bool
accept_request::start_async_op_impl()
{
  //fprintf(stderr,"accept_request %p: start async_op_impl\n",this);
  bool failed = (start(*pd) == -1);      // accept_control_block function
  if(failed)
    fprintf(stderr, "FAILED TO SPAWN ACCEPT REQUEST\n");
  //else
  //  fprintf(stderr, "ACCEPT REQUEST SPAWNED\n");
  return failed;
}

void
accept_request::wakeup(posix_demuxer& demux)
{
  // does the leg work.
  accept_control_block::wakeup(demux);
  //'fprintf(stderr, "faio_posix::accept_request::wakeup\n");

  if(accepted == -1)
  {
    // I don't know if this is a good idea...
    fprintf(stderr, "accept request failed (%i), retrying...\n",
      socket_err);
    // didn't get it - go back to sleep
    if(start(demux) == -1)
      fprintf(stderr, "failed again... probably was a bad idea\n");
    return;
  }

  notify_finished();
}

// from driver request
flxfileio_request::~flxfileio_request(){}
flxfileio_request::flxfileio_request(){}


bool
flxfileio_request::start_async_op_impl()
{
  //fprintf(stderr,"flxfileio_request: start async_op_impl\n");
  // printf("driver called fileio start_async_op code\n");

  // need to create the async io thing here, or ask the driver for it
  // driver needs to go a little less portable
  aio_worker->add_worker_task(this);

  return false;       // no wakeup
}

void
flxfileio_request::finished() { notify_finished(); }
}}

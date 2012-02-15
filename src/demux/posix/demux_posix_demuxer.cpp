#include "demux_posix_demuxer.hpp"
#include "demux_sockety.hpp"
#include "demux_quitter.hpp" // fns for waking and quitting a demuxer

#include <stdio.h>        // "printf"
#include <assert.h>       // assert
#include <string.h>       // strerror
#include <unistd.h>       // close

#include <sys/types.h>      // send/recv
#include <sys/socket.h>

//#include <sys/errno.h>
#include <errno.h>        // GUSI & solaris prefer this

namespace flx { namespace demux {

posix_demuxer::~posix_demuxer()
{
}

bool
posix_demuxer::socket_recv(int s, sel_param* pb)
{
  //fprintf(stderr,"posix_demuxer:socket_recv req=%ld\n", (long)pb->buffer_size);
  // why do I have the zero buffer size?
  assert(pb->buffer_size > pb->bytes_written || 0 == pb->buffer_size);
  ssize_t     nbytes;

  // if this were read then this fn would work with non-sockets
  nbytes = recv(s, pb->buffer + pb->bytes_written,
        pb->buffer_size - pb->bytes_written, 0);

  //fprintf(stderr,"posix_demuxer:socket_recv got %d bytes\n", nbytes);
  /*
  fprintf(stderr,"posix_demuxer RECV: s=%d, pb=%p, buf+%d, req=%d, got %d\n",
    s,pb, int(pb->bytes_written), int(pb->buffer_size - pb->bytes_written), int(nbytes)
  );
  */
  if(nbytes <= 0) // so what happens if the client wants to send 0 bytes?
  {
    if(nbytes == 0)
    {
      pb->eof_detected = true;
      return true;        // connection closed
    }
    else
    {
      perror("recv");       // can get reset connection here
      pb->eof_detected = true;
      pb->error_detected = true;
      return true;        // so say closed, yeah?
    }
  }
  else
  {
    // got some data
    pb->bytes_written += nbytes;
  }
  return false;           // connection didn't close
}

bool
posix_demuxer::socket_send(int s, sel_param* pb)
{
  // kqueue (and some of the other ones) can let you know know how much
  // to write... imagine that!

  // why do I have the zero buffer size?
  //fprintf(stderr,"posix_demuxer:socket_send req=%ld, written=%ld\n", (long)pb->buffer_size, pb->bytes_written);
  assert(pb->buffer_size > pb->bytes_written || 0 == pb->buffer_size);

  ssize_t     nbytes;

  //fprintf(stderr,"posix_demuxer:socket_send\n", nbytes);
  nbytes = send(s, pb->buffer + pb->bytes_written,
    pb->buffer_size - pb->bytes_written, 0);
  /*fprintf(stderr,"posix_demuxer:socket_send wrote %ld bytes from offset %ld out of %ld to be written\n", 
       (long) nbytes,
       (long) pb->bytes_written,
       (long) pb->buffer_size - pb->bytes_written
  );
  */
  /*
  fprintf(stderr,"posix_demuxer SEND: s=%d, pb=%p buf+%d, req=%d, got %d\n",
    s,pb, int(pb->bytes_written), int(pb->buffer_size - pb->bytes_written), int(nbytes)
  );
  */
  // similar story here, with send vs write?

  // what's the story with zero? Is that allowed or does it signal
  // that the connection closed?
  // JS: According to the man page, it can happen on an async socket
  // but for us this could be if the notification event lied
  // OR a socket connection got closed in between the notification
  // and this call: we can tell by looking at errno but that utterly
  // sucks .. oh well, unix is a pretty bad OS in some ways
  // OR .. it could happen if the client decided to send 0 bytes!

  if(-1 == nbytes)
  {
    fprintf(stderr,"posix_demuxer: socket send failed, connection closed by client?\n");
    perror("send");
    fprintf(stderr,"Should have printed the error code above ..\n");
    pb->eof_detected = true;
    pb->error_detected = true; // really, trying to write on a connection merely closed
                               // by the client is NOT an error, since there's no other way
                               // to tell than try to do a write
    return true;          // I guess the connection closed
  }
  else
  {
    // sent some data
    pb->bytes_written += nbytes;
  }
  return false;           // connection didn't close
}

// get a posix demuxer to quit, that is, get the demuxer's event thread
// to exit. doesn't return until this has happened. pretty sure that
// calling this on a demuxer used synchronously would be a bad idea.
// confirmed, when there is no other thread waiting on the demuxer
// we wait for ever in quit
// doesn't throw
void
posix_demuxer::async_quit()
{
  try {
    // NEW and IMPROVED!!! demux quitter which sets demux quit flag
    // via self pipe trick then waits for self pipe/quitting callback
    // to finish. no fear of quitter being destructed early!
    // fprintf(stderr, "async_quit called on posix demuxer\n");
    demux_quitter quitter;
    quitter.quit(this);
        // event thread has exited at this point
  } catch(...) {
    fprintf(stderr, "error waking demuxer with self pipe quitter\n");
  }
}

#if 0
  //nbytes = recv(s, pb->buffer + pb->bytes_written,
  //      pb->buffer_size - pb->bytes_written, 0);

  // select and kqueue know when non socket fds have data.
  // recv only works with sockets, but read works with both files
  // and sockets and who knows what else. is there any disadvantage
  // to using read instead? apart from losing flags arg?
  // does read get the same 0 bytes = close behaviour
  nbytes = read(s, pb->buffer + pb->bytes_written,
        pb->buffer_size - pb->bytes_written);
#endif

// handy posix control blocks for accept, connect.

int
accept_control_block::start(posix_demuxer& demux)
{
  // add listener to demuxer as reading socket - see man 2 accept
  // returns 0 on success, -1 on failure. not sure how to communicate
  // the error.
// could try the accept now, to see if it succeeds instantly...
// observe wakeup rules (formulate them first)
  accepted = -1;
  // socket_err = 0;
  // not quite true, but I want it to be clear if this ever becomes possible
  // to do immediately
  socket_err = EINPROGRESS;
    return demux.add_socket_wakeup(this, PDEMUX_READ);
}

// one wakeup socket is in accepted and error in socket_err
void
accept_control_block::wakeup(posix_demuxer& demux)
{
  // fprintf(stderr,"accept_control_block woke up\n");

  // we can now accept without blocking
  // s is the listener, ambiguously named in parent socket_wakeup class
  accepted = nice_accept(s, &socket_err);

  if(accepted == -1)
  {
    fprintf(stderr, "nice_accept failed, err (%i)\n", socket_err);
  }
}

// returns -1 on failure, 0 on success. on success the call is finished
// (and so no wakeup) if socket_err == 0.
int
connect_control_block::start(posix_demuxer& demux)
{
  // fprintf(stderr,"async connect start\n");

  int finished;

  // returns either finished and err, or not finished
  // and (no err || EINPROGRESS)
  s = async_connect(addy, p, &finished, &socket_err);

  // fprintf(stderr,"async_connect returned s: %i, finished: %i, err=%i\n",
  //  s, finished, socket_err);

  if(-1 == s)   // failed!
  {
    fprintf(stderr,"async_connect failed (%i)\n", socket_err);
    return -1;  // error in socket_err, no wakeup
  }

  if(finished)
  {
    // this actually happens on solaris when connecting to localhost!
    fprintf(stderr,"async_connect finished immediately, waking\n");
    fprintf(stderr, "No wakeup coming...\n");
    // this does not indicate an error, but that there is no wakeup
    // coming. this could be done by a wakeup, all that happens is
    // getsockopt is called to check the socket's error state.
    return -1;
  }

  // fprintf(stderr,"connect_request didn't finish immediatly, sleeping\n");

  // add to demuxer as writing socket - see man 2 connect
  // how do they get the error?
    return demux.add_socket_wakeup(this, PDEMUX_WRITE);
}

void
connect_control_block::wakeup(posix_demuxer& demux)
{
  // fprintf(stderr,"connect woke up\n");
  // this is how we check the success of async connects
  // if get_socket_err fails, we're treating its errno as the socket's...
  if(get_socket_error(s, &socket_err) == -1)
    fprintf(stderr, "eep - get_socket_err failed!\n");

  // failed, throw away socket
  if(0 != socket_err)
  {
    fprintf(stderr,"async connect error: %s (%i), closing\n",
      strerror(socket_err), socket_err);
    // we created the connect socket, so we close it too.
    if(close(s) != 0)
      perror("async socket close");

    s = -1;   // the result
  }

  // resulting connected socket in s
}
}}

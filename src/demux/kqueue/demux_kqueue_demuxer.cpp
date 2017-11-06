// kqueue demuxer for bsd/os x
// N.B. calling close on a file descriptor will remove any kevents that
// reference that descriptor. that would explain remove complaining from
// time to time.
// try EV_EOF to pick up eofs, useful for async file io.

#include "demux_kqueue_demuxer.hpp"

#include <stdio.h>      // perror
#include <errno.h>      // errno, for debugging
#include <unistd.h>     // close

#include <sys/types.h>    // from the kqueue manpage
#include <sys/event.h>    // kernel events
#include <sys/time.h>   // timespec (kevent timeout)

// #include <sys/syscall.h> // syscall(SYC_close,kq) close workaround

namespace flx { namespace demux {
kqueue_demuxer::kqueue_demuxer()
  : kq(-1)
{
fprintf(stderr, "OSX Kqueue demuxer starting\n");
  // Not that you care, but this event queue is not inherited by
  // forked children.
  kq = kqueue();
  if(-1 == kq)
  {
    perror("kqueue");
    throw -1;
  }
}

kqueue_demuxer::~kqueue_demuxer()
{
fprintf(stderr, "OSX Kqueue demuxer ending\n");
  // calling close on a kq while a thread is waiting in kevent causes close
  // to block! this happens on 10.4. Hard to say on 10.3 as close simply
  // fails there. we need to wake the waiting thread, so we'll use that
  // handy self pipe waker. Luckily, kqueues are responsive to new fds,
  // otherwise we'd need the self pipe waker to be there from the start
  // p.s. it's also bad form to destruct a demuxer while a thread waits
  // on it. top marks to kqueues for making this obvious, passing fail
  // to me for not applying the same to the other async demuxers.
  async_quit();

  //if(syscall(SYS_close, kq) == -1)
  // I don't seem to be able to close a kq. can't fstat it either
  if(-1 != kq && close(kq) == -1)
    perror("kqueue close");
}


// Events of interest to us ERead, EWrite.
// ERead has fflags: NOTE_LOWAT, NOTE_EOF. ident is a descriptor (any?)

// if you're using the kqueue_demuxer to do a single biderectional wakeup,
// be aware that it currently breaks the "one shot" rule, that is you
// make get an unexpected wakeup the next time you call wait.
int
kqueue_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  //fprintf(stderr,
  //  "Add socket wakeup: socket: %d Operation: %s\n",
  //  sv->s, flags==PDEMUX_READ ? "Read" : "Write"
  //);
  // we only know these flags
  if((flags & ~(PDEMUX_READ | PDEMUX_WRITE))) return -1;

// could set wakeup_flags here of what's been installed!

  // FUCK - can only do one at a time with kqueues
  // For those doing both, if one fails, you're in a bit of trouble.
  if(flags & PDEMUX_READ)
  {
    if(add_kqueue_filter(sv, EVFILT_READ) == -1) return -1;
    //fprintf (stderr,"Added read filter\n");
  }

  if(flags & PDEMUX_WRITE)
  {
    if(add_kqueue_filter(sv, EVFILT_WRITE) == -1) return -1;
    //fprintf(stderr,"Added write filter\n");
  }

  return 0;
}

int
kqueue_demuxer::add_kqueue_filter(socket_wakeup* sv, short filter)
{
  int       s = sv->s;
  struct kevent evt;

  // this works just like select if the s is a listening socket
  // *except* works with all types of fds, including pipes, files & fifos
  // can set low water mark for reads with NOTE_LOWAT in fflags and size
  // in data. on return data contains number of bytes available to read
  // on return sets EV_EOF in flags if read dir socket has shutdown and
  // returns a possible socket err in fflags
  // should that be EV_ENABLE | EV_ADD. fflags zero cos I don't know what
  // to put there. pass pb in udata

  // adding EV_ONESHOT to save me removing on wakeup (a syscall).
  // I now require that during the evt be removed before wakeup fn.

  EV_SET(&evt, s, filter, EV_ADD | EV_ONESHOT, 0, 0, sv);
  // trying to detect when have reached eof with async file io using kq
  //EV_SET(&evt, s, EVFILT_READ, EV_ADD, | EV_ONESHOT NOTE_LOWAT, 16*1024, sv);

  // add event
  int err;
  if((err=kevent(kq, &evt, 1, NULL, 0, NULL)) < 0)
  {
    fprintf(stderr, "kevent returned code %d, errno=%d\n",err,errno);
    perror("kevent add_kqueue_filter");
    return -1;
  }
  return 0;
}

// useful, but unused atm, thanks to ONESHOT.
// this function skirts the portability boundaries of the demux interface
// for kqueues each event monitor is identified (for sockets) by a pair,
// (s, filter), or for us, (s, {in|out}). This means that we can add
// individual wakeups for reading and writing but not both at once.
// I think epoll can do both, and so can select (and N/A to IOCPs).
// This "both at once" thing can't easily be one shot. There's a good
// case for this behaviour to be defined "undefined". Not many people
// using this part - could be caveat emptor...
int
kqueue_demuxer::remove_kqueue_filter(int s, short filter)
{
  struct kevent evt;

  EV_SET(&evt, s, filter, EV_DELETE, 0, 0, NULL);
  if(kevent(kq, &evt, 1, NULL, 0, NULL) < 0)
  {
    perror("kevent remove_socket_wakeup");
    return -1;
  }
  //fprintf(stderr,"Removed kqueue filter\n");
  return 0;
}

int
kqueue_demuxer::remove_socket_wakeup(int s, int flags)
{
  int r1 = 0, r2 = 0;

  if(flags & PDEMUX_READ) r1 = remove_kqueue_filter(s, EVFILT_READ);
  if(flags & PDEMUX_WRITE) r2 = remove_kqueue_filter(s, EVFILT_WRITE);

  // If you want to know which one failed, you're out of luck.
  if(r1 || r2) return -1;

  return 0;
}

// from "advanced macos programming", on reading shutdown causes
// the EV_EOF flag to be set in the flags field and returns errno
// in the fflags field. There may still be pending data to read
// when EV_EOF is set. The data field says how many bytes available.
// for writing data says how much you can write. EV_EOF is set
// when the reader "disconnects". Says nothing about errno/fflags
// in this case.
/*
    fprintf(stderr,"readevt on %i, EOF = %s\n",
      s, (ev.flags & EV_EOF) ? "TRUE" : "FALSE");
 */

// do that thing where you get the events. can I get them one at a time?
// I bet I can.
void
kqueue_demuxer::get_evts(bool poll)
{
  // event seems to remain unless we remove it
  struct kevent ev;
  int       nevts;

  struct timespec timeout, *tptr = NULL;

  if(poll)
  {
    timeout.tv_sec = 0;   // effectuate a poll
    timeout.tv_nsec = 0;
    tptr = &timeout;
    fprintf(stderr,"Kqueue emulating poll\n");
  }

  // timeout.tv_sec = 1;    // timeout every second
  // timeout.tv_nsec = 0; // 10^9 nanoseconds per second

  nevts = kevent(kq, NULL, 0, &ev, 1, tptr);  // wait or poll

  if(nevts <= 0)
  {
    // error, else timeout & return to allow cancel
    if(nevts < 0)
      perror("kevent event fetch");

    return;
  }

  // fprintf(stderr,"kqueue wakeup!\n");
  //fprintf(stderr,"Kqueue got %d events\n",nevts);
  socket_wakeup*  sv = (socket_wakeup*)ev.udata;
  //fprintf(stderr, "Socket Event: socket: %d\n", sv->s);
  // The filters are not bit fields, hence they must come in serially.
  // this means you're never going to get both read and write on
  // a kqueue_demuxer wake up. No worries.
  if(ev.filter == EVFILT_READ)
  {
    // fprintf(stderr, "Socket READ: socket: %d\n", sv->s); 
  // this capability is lost for the moment, as we have no way
  // of explaining it to felix. the event stuff isn't so good right now
/*
    // can chunk up on accepts. nice one kqueue
    if(NULL == sv)      // => listener
    {
      int backlog = (int)ev.data;
      // fprintf(stderr,"kq listen backlog: %i\n", backlog);
      for(int i = 0; i < backlog; i++) handle_connection();
    }
    else
*/
    // If a socket wakeup were a control block, you'd set the err here.
    if(ev.flags & EV_EOF)
    {
      // errno in fflags!
      /* this isn't actually an error, just a transient eof ?? 
      fprintf(stderr,
        "got EV_EOF on read, %i bytes remain in buffer, errno=%i\n",
        (int)ev.data, ev.fflags);
      */
    }
    // fprintf(stderr,"EVFILT_READ: got %i bytes coming\n", (int)ev.data);
    // remove_reading_fd(s);      // now useing EV_ONESHOT
// remove other outstanding here...?
    sv->wakeup_flags = PDEMUX_READ;   // Tell 'em what they've won.
    sv->wakeup(*this);
  }
  else if(ev.filter == EVFILT_WRITE)
  {
    //fprintf(stderr, "Socket WRITE: socket: %d\n", sv->s); 
    // fprintf(stderr,"EVFILT_WRITE: can write (?) %i bytes\n", (int)ev.data);

    // using oneshot mode now.
    // remove_writing_fd(s);

    if(ev.flags & EV_EOF)
    {
      // errno in fflags? data should be zero bytes, right?
      // can't write anything
      fprintf(stderr,
        "got EV_EOF on write, socket = %d, data bytes =%i (0?), errno/fflags?=%i\n",
        sv->s, (int)ev.data, ev.fflags);
    }
// remove other outstanding here?
    sv->wakeup_flags = PDEMUX_WRITE;
    sv->wakeup(*this);
  }
  else
  {
    fprintf(stderr,"unsolicited event from kqueue (%i)...\n", ev.filter);
    // no wakeup
  }
}
}}

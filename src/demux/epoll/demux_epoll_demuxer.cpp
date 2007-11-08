// epoll interface. does epoll support ordinary files in addition to sockets?
// EPOLLET to make epoll edgetriggered. I guess the default is level triggered.

// epoll events are not one shot, in fact they're quite sticky so socket
// filters must be removed manually to guarantee a one-to-one wakeup
// to add_wakeup ratio. note that the oneshot flag is not a solution.

// cool! EPOLLONESHOT
// BUGGER! doesn't seem to exist! and doing this doesn't make it so!
// #ifndef EPOLLONESHOT
// #define EPOLLONESHOT (1<<30)
// #endif

#include "demux_epoll_demuxer.hpp"

#include <sys/epoll.h>  // for epoll_*
#include <stdio.h>    // for perror
#include <unistd.h>   // for close
#include <errno.h>    // EEXIST, errno

namespace flx { namespace demux {

epoll_demuxer::epoll_demuxer()
  : epoll_fd(-1)
{
  // EPOLLONESHOT is shit, don't use it. Enabling it just means that your
  // wakeups are suppressed and you have to use EPOLL_CTL_MOD instead
  // of EPOLL_CTL_ADD. If it isn't defined then so much the better.
//#ifdef EPOLLONESHOT
//  fprintf(stderr,"WARNING: EPOLLONESHOT AVAILABLE (%x)!!!\n", EPOLLONESHOT);
//#endif

  // god knows what the maximum size will be, I'll just say 1 for now
  epoll_fd = epoll_create(1);
  if(-1 == epoll_fd)
  {
    perror("epoll_create");
    throw -1;
  }
}

epoll_demuxer::~epoll_demuxer()
{
  async_quit(); // get waiting thread to exit.

  if(-1 != epoll_fd)
  {
    if(close(epoll_fd) != 0)
      perror("epoll close");
  }
}

int
epoll_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  int s = sv->s;

  struct epoll_event  evt;
  // fprintf(stderr,"add_socket_wakeup: %i (sv=%p, flags=%x)\n",
  //  s, sv, flags);

  // EPOLLONESHOT saves us not only a system call to remove epoll evts,
  // which aren't intrinsically one-shot, but having to do it ourselves
  // would have been a pain as epoll doesn't tell you which fd had the event
  // this way we can get away with not knowing & not losing our user cookie
  evt.events = 0;

  if(flags & PDEMUX_READ) evt.events |= EPOLLIN;
  if(flags & PDEMUX_WRITE) evt.events |= EPOLLOUT;

  // fprintf(stderr, "flags %x -> evt.events %x\n", flags, evt.events);

  // We do the remove manually because oneshot in epoll doesn't
  // remove the socket, but rather, disables it.
//#ifdef EPOLLONESHOT
//  evt.events |= EPOLLONESHOT;         // yes!
//#endif
  // I think EPOLLHUP comes when the connection closes (on read?)
// poll's (plain old poll) equivalents to this are ignored for input
// same here?
  // I get EPOLLHUPs for bad async connects whether I ask for them or not.
  evt.events |= (EPOLLHUP | EPOLLERR);    // I think I want this

  evt.data.ptr = sv;              // our user data

  if(epoll_ctl(epoll_fd, EPOLL_CTL_ADD, s, &evt) == -1)
  {
    // EPOLL_CTL_MOD cannot help us do bidirection io on one socket,
    // the mod will overwrite the old user cookie and direction.
    // It seems that only kqueues, select and iocps allow that.
    // Will need a wakeup that can do both, oneshot that indicates
    // the available direction.
// when using oneshot, we're supposed to use EPOLL_CTL_MOD
#if 0
    int err = errno;

    if(EEXIST == err)
    {
      // ok - let's try EPOLL_CTL_MOD
      fprintf(stderr, "RETRYING WITH EPOLL_CTL_MOD\n");
      if(epoll_ctl(epoll_fd, EPOLL_CTL_MOD, s, &evt) != -1)
        return 0; // good!
    }
#endif
    perror("epoll_ctl (add)");

    return -1;
  }
  return 0;
}

// epoll doesn't differentiate on events. I bet I could
// just not pass that event...
void
epoll_demuxer::remove_wakeup(int s)
{
  // EPOLL_CTL_DEL uses no information from the event
  // and so I should be able to pass NULL.
  struct epoll_event evt;
  // evt.events = (read) ? EPOLLIN : EPOLLOUT;

  // fprintf(stderr,"removing socket wakeup %i\n", s);

  if(epoll_ctl(epoll_fd, EPOLL_CTL_DEL, s, &evt) == -1)
  {
    //const char* str = (read) ? "epoll_ctl (remove read)"
    //  : "epoll_ctl (remove write)";
    // perror(str);
    perror("epoll_ctl (remove)");
  }
}

void
epoll_demuxer::get_evts(bool poll)
{
  struct epoll_event  evt;

  switch(epoll_wait(epoll_fd, &evt, 1, (poll) ? 0 : ~0))
  {
    case -1:    // error
    perror("epoll_wait");
      // fall through
    case 0:     // no events (happens with timeout)
      return;
  }

  socket_wakeup* sv = (socket_wakeup*)evt.data.ptr;

  // not seeing timeouts as they're filtered by the switching.
  // assuming that sv is good
  // fprintf(stderr,"wakeup (sv=%p, sv->s=%i evt.events=%x)!\n",
  //  sv, sv->s, evt.events);

  // accumulate bit field of what we got
  sv->wakeup_flags = 0;

  bool  wake = false;

  // it might be possible to get both a read & write event...
  // in which case I should take out the else below
  if(evt.events & EPOLLIN)                // I think this is how you do it
  {
    // fprintf(stderr,"EPOLLIN for %p\n", sv);
    sv->wakeup_flags |= PDEMUX_READ;
    wake = true;
  }

  if(evt.events & EPOLLOUT)
  {
    //fprintf(stderr,"EPOLLOUT for %p\n", sv);
    sv->wakeup_flags |= PDEMUX_WRITE;
    wake = true;
  }

  // Is this for shutdown or closing of the other end?
  // I get it for failed async connects. I don't know if other events cause
  // it. In any case, I don't know whether it should be for read or write,
  // so I just don't say. In any case, it should wake to get error.
  // I seem to get both EPOLLHUP and EPOLLERROR on bad async connect
  // see poll demuxer notes on POLLHUP for further possibly useful info.
  if(evt.events & EPOLLHUP)
  {
    fprintf(stderr, "EPOLLHUP for %p->%i\n", sv, sv->s);
    sv->wakeup_flags |= PDEMUX_EOF;
    wake = true;
  }

  if(evt.events & EPOLLERR)
  {
// How do I retrieve the error?
    // There's no ambiguity - there's only ever one fd in a given epoll.
    // If oneshot's present then don't need to do anything
// not sure what to do here. if we've enabled/got oneshot the socket
// should already have been removed
    fprintf(stderr,"epoll error, waking: %i (errno?)\n", sv->s);
    // similar story to EPOLLHUP
    sv->wakeup_flags |= PDEMUX_ERROR;
    wake = true;
  }

    if((evt.events & ~(EPOLLERR|EPOLLIN|EPOLLOUT|EPOLLHUP)))
    {
        fprintf(stderr,"unknown events in epoll_demuxer %x\n", evt.events);
    }

  // we got something. tell the people.
  // not dependent solely on wakeup_flags - errors need to wake too.
  if(wake)
  {
    // we got something. better call wakeup, must remove to guarantee
    // 1-1 wakeups with add_sockets
    // fprintf(stderr, "no one-shot... remove %i\n", sv->s);
    remove_wakeup(sv->s);
    // fprintf(stderr, "calling wakeup (flags=%x)\n", sv->wakeup_flags);
    sv->wakeup(*this);
  }
}
}}

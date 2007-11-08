// Evtports can get timer wakeups with PORT_SOURCE_TIMER.
// Can also pick up aio notifications with PORT_SOURCE_AIO.

// looks like this stuff is only in solaris10, and not SunOS 5.8. Damn.

#include "demux_evtport_demuxer.hpp"

#include <port.h>
#include <poll.h> // POLLIN/POLLOUT
#include <stdio.h>  // printf
#include <unistd.h> // close
#include <assert.h>

namespace flx { namespace demux {

// header files for this stuff?
// can use port_send for user defined events, to wake up reap loop
// truss to see what's happening

evtport_demuxer::evtport_demuxer()
{
  if((evtport = port_create()) < 0)
  {
    perror("port_create");
    throw -1;
  }
}

evtport_demuxer::~evtport_demuxer()
{
  async_quit(); // gets waiting thread to quit, returning afterwards

  if(-1 != evtport)
  {
    if(close(evtport) != 0)
      perror("evtport close");
  }
}

int
evtport_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  if(flags & ~(PDEMUX_READ|PDEMUX_WRITE)) // I can't understand you, lady!
    return -1;

  int events = 0; // events are flags so we can accumulate them
  int s = sv->s;

  if(flags & PDEMUX_READ) events |= POLLIN;
  if(flags & PDEMUX_WRITE) events |= POLLOUT;

  // POLLHUP might make sense only for reads... add conditionally?
  events |= (POLLHUP | POLLERR);

  // fprintf(stderr,"add_socket_wakeup: %i, sv: %p (%x)\n", s, sv, flags);

  // register for event we are interested in...
  // works for files, sockets, timers...
  // sockets are file descriptors in unix, so src is PORT_SOURCE_FD
  if(port_associate(evtport, PORT_SOURCE_FD, (uintptr_t)s, events, sv) == -1)
  {
    perror("add socket_wakeup/port_associate");
    return -1;
  }

  return 0;
}

// note that these two functions are exactly the same
// we have to remove after a read or write else we can get multiple
// wakeups - usually with a dud user cookie. the fact that there is
// no differentiation between POLLIN & POLLOUT could be a problem for
// mixed read/write things (rare). note that evt_ports let me associate
// the samething twice. I don't know if this means you have to dissociate
// (disassociate) twice.
void
evtport_demuxer::remove_wakeup(int s)
{
  if(port_dissociate(evtport, PORT_SOURCE_FD, s) == -1)
    perror("reading port_dissociate");
}

#define POLLPR(ev) if(e->portev_events & ev) fprintf(stderr,#ev", ")

static void
print_port_evt(port_event_t* e)
{
  char* srcstr[PORT_SOURCE_ALERT-PORT_SOURCE_AIO+1]
    = { "ALERT", "TIMER", "USER", "FD", "AIO"};
  fprintf(stderr,"e: %p\n\t", e);
  //fprintf(stderr,"portev_events: %x\n\t", e->portev_events);
  fprintf(stderr,"portev_events: ");

  // I got these constants from the poll.h file
  POLLPR(POLLIN); POLLPR(POLLOUT); POLLPR(POLLPRI);
  POLLPR(POLLRDNORM); POLLPR(POLLRDBAND); POLLPR(POLLWRBAND);

  // in poll these are in a different field. port_event_t doesn't
  // have that field, so lets try here.
  POLLPR(POLLERR); POLLPR(POLLHUP); POLLPR(POLLNVAL); POLLPR(POLLREMOVE);

  fprintf(stderr," (%x)\n\t", e->portev_events);

  int src = e->portev_source;
  if(PORT_SOURCE_AIO <= src && src <= PORT_SOURCE_ALERT)
  {
    fprintf(stderr,"portev_source: PORT_SOURCE_%s (%x)\n\t",
      srcstr[src-PORT_SOURCE_AIO], src);
  }
  else
  {
    fprintf(stderr,"portev_source: %x\n\t", e->portev_source);
  }

  fprintf(stderr,"portev_pad: %x\n\t", e->portev_pad);
  // often our socket
  fprintf(stderr,"portev_object: %x\n\t", e->portev_object);
  fprintf(stderr,"portev_user: %p\n", e->portev_user);
}

void
evtport_demuxer::get_evts(bool poll)
{
  // Block until a single event appears on the port. Event will not fire
  // again, so we get max 1 wakeup per event added.

  port_event_t  evt;
  timespec    timeout, *tp = NULL;

  if(poll)    // effect a poll
  {
    timeout.tv_sec = 0;
    timeout.tv_nsec = 0;
    tp = &timeout;
  }

  // wait for single event, no timeout
  if(port_get(evtport, &evt, tp) == -1)
  {
    perror("port_get");
    return;
  }

  // fprintf(stderr,"PORT_GET RETURNED: "); print_port_evt(&evt);

  // get wakeup obj tucked away in the user cookie.
  socket_wakeup*  sv = (socket_wakeup*)evt.portev_user;
  int       s = evt.portev_object;

  assert(sv != NULL);
  if(evt.portev_source != PORT_SOURCE_FD)
  {
    // when polling I often end up in here - we get an unknown evt
    // source and a POLLNVAL event plus lots of other unknown flags.
    // there's interesting looking stuff in the user field and so on,
    // but it's nothing of mine and also undocumented
    // fprintf(stderr,"got non PORT_SOURCE_FD (s=%i, sv=%p, src=%i)\n",
    //  s, sv, evt.portev_source);
    // fprintf(stderr, "skipping out...\n");
    return;
  }


  // let's see what we've got for the wakeup
  sv->wakeup_flags = 0;

  if(evt.portev_events & POLLERR)
  {
    fprintf(stderr,"ERRORS on s = %i, sv = %p\n", s, sv);
    //evt.portev_events &= ~POLLERR;
    //return;
  }

  // for bidirectional wakeups, we should be able to get both
  // POLLIN and POLLOUT at the same time, but I've not yet
  // seen it happen, they're coming in one at a time for me.


  if(evt.portev_events & POLLIN)
  {
    // fprintf(stderr,"GOT POLLIN FOR %p\n", sv);
    sv->wakeup_flags |= PDEMUX_READ;
  }

  if(evt.portev_events & POLLOUT)
  {
    // fprintf(stderr,"GOT POLLOUT FOR %p\n", sv);
    sv->wakeup_flags |= PDEMUX_WRITE;
  }

  // I never asked for POLLERR, but anyway
  if(evt.portev_events & ~(POLLIN | POLLOUT | POLLERR))
    {
        fprintf(stderr,"UNSOLICITED events in evtport_demuxer (%x)\n",
      evt.portev_events);
    }

  assert(sv->wakeup_flags != 0);    // we should've gotten SOMETHING.

  if(sv->wakeup_flags)
    sv->wakeup(*this);
}
}}

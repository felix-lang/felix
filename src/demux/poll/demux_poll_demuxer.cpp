#include <stdio.h>      // my friend printf
#include <poll.h>
#include <assert.h>
#include "demux_poll_demuxer.hpp"

#include <vector>

namespace flx { namespace demux {

using namespace std;

typedef vector<socket_wakeup*> sockvec;
typedef vector<struct pollfd> fdvec;

#define FDS ((fdvec*)fd_array)
#define SOCS ((sockvec*)sv_array)

// be aware that under os x 10.3 (and other systems?), poll is a thin
// user level layer on top of select and so of no real advantage.
// the select emulation doesn't seem to give POLLHUP errs, either.
// under 10.4 poll doesn't appear to be calling select, so I guess it's
// all good.

poll_demuxer::poll_demuxer()
  : fd_array(0), sv_array(0)
{
  // fprintf(stderr, "poll_demuxer ctor\n");
}

poll_demuxer::~poll_demuxer()
{
  // fprintf(stderr, "poll_demuxer dtor\n");
  if(SOCS) delete SOCS;
  if(FDS) delete FDS;
}

// breaking up for a thread safe impl
void
poll_demuxer::get_arrays(void** fds, void** svs)
{
  *fds = fd_array;
  *svs = sv_array;

  fd_array = 0;
  sv_array = 0;
}

// returns nevents
int
poll_demuxer::dopoll(void* infds, bool poll_flag)
{
  fdvec*    fds_copy = (fdvec*)infds;

  if(!fds_copy)
  {
    if(!poll_flag) fprintf(stderr, "Warning ::poll(\\inf) on zero fds!\n");
    return 0;
  }

  struct pollfd*  fds = &(*fds_copy)[0];
  // I sometimes end up with nothing to watch in poll mode
  // this type doesn't seem to exist for the 10.3 emulated poll.
  unsigned long nfds = (*fds_copy).size();
  // nfds_t  nfds = (*fds_copy).size();
  int      nevts;

  // fprintf(stderr, "calling ::poll with %p*%i fds\n", fds, nfds);
  // -1 for timeout means wait indefinitely
  nevts = ::poll(fds, nfds, (poll_flag) ? 0 : -1);

  if(-1 == nevts)
  {
    perror("poll_demuxer::get_evts");
    return 0;
  }

  return nevts;    // zero => timeout
}

// processes events, calls callbacks, deletes fds & svs upon completion.
// Seems a bit busy, no?
void
poll_demuxer::process_evts(void* infds, void* svs, int nevts)
{
  // Optimisation: when no events (due to timeout) and our fds
  // are null (nothing changed in the meantime), just set the
  // fd and svs members to the incoming ones and return

  // fprintf(stderr, "poll::process_evts: %i, %p\n", nevts, fd_array);

  if(0 == nevts && !fd_array)
  {
    // fprintf(stderr, "Optimising by resetting arrays!\n");
    assert( !sv_array );  // keep in sync
    fd_array = infds;
    sv_array = svs;
    return;
  }

  fdvec*    fds_copy = (fdvec*)infds;
  sockvec*  socs_copy = (sockvec*)svs;

  struct pollfd*  fds = &(*fds_copy)[0];
  unsigned long nfds = (*fds_copy).size();
  // nfds_t  nfds = (*fds_copy).size();

  // sanity check. looks like read and write count as 1 each
  int    evts_encountered = 0;

  // examine all fds for signs of life. try early out with nevts?
  // for(nfds_t i = 0; i < nfds; i++, fds++)
  for(unsigned long i = 0; i < nfds; i++, fds++)
  {
    // fprintf(stderr, "fds[%i]->revents=%x\n", i, fds->revents);

    socket_wakeup* sv = (*socs_copy)[i];

    // accumulate bit field of what we got
    // don't touch original bits, we might have to restore them
    int  wakeup_flags = 0;

    bool    wake = false;

    // it might be possible to get both a read & write event...
    // in which case I should take out the else below
    if(fds->revents & POLLIN)                // I think this is how you do it
    {
          // fprintf(stderr,"POLLIN for %p->%i\n", sv, sv->s);
      wakeup_flags |= PDEMUX_READ;
      wake = true;
      evts_encountered++;
    }

    if(fds->revents & POLLOUT)
    {
      // fprintf(stderr,"POLLOUT for %p->%i\n", sv, sv->s);
      wakeup_flags |= PDEMUX_WRITE;
      wake = true;
      evts_encountered++;
    }

    // check here for the unsolicited POLLERR, POLLHUP and POLLNVALs
    if(fds->revents & POLLERR)
    {
      fprintf(stderr, "POLLERR for %p->%i\n", sv, sv->s);
      wakeup_flags |= PDEMUX_ERROR;
      wake = true;    // good to do?
    }

    // device has been disconnected. this and POLLOUT are mutually exclusive.
    // a stream can never be writeable again if a hangup has occured.
    // I've seen POLLHUPs come in for shutdown(s, 1). In this case you want
    // the wakeup, at least if you were waiting to write. POLLHUPs also seem
    // to be the message/wake up when reading from a connection that has
    // closed: you get the remaining bytes, but via POLLHUP rathern POLLOUT.
    // perhaps not worth printing, seeing as this usage is quite common
    if(fds->revents & POLLHUP)
    {
      fprintf(stderr, "POLLHUP for %p->%i\n", sv, sv->s);
      assert((fds->revents & POLLOUT) == 0);
      wakeup_flags |= PDEMUX_EOF;
      wake = true;    // good to do? probably.
    }

    // Invalid fd. We shouldn't ever get that.
    if(fds->revents & POLLNVAL)
    {
      fprintf(stderr, "POLLNVAL for %p->%i\n", sv, sv->s);
      wake = true;    // good to do?
    }

    if(wake)
    {
      // 1-1 wakeups with add_sockets
      // be aware that callback may add back...
      sv->wakeup_flags = wakeup_flags;
      sv->wakeup(*this);
    }
    else
    {
      // reinstall for the next iteration. note that we keep a copy
      // of the flags in sv->wakeup_flags, set on adding. that belongs
      // to us so there should be no problem there.
      //fprintf(stderr, "poll::readding: %i, %x\n",
      //  sv->s, sv->wakeup_flags);
      if(add_socket_wakeup(sv, sv->wakeup_flags) == -1)
        fprintf(stderr, "poll re-add finished immediately!?!\n");
    }
  }

  // keep the bastards honest
  if(evts_encountered != nevts)
  {
    fprintf(stderr, "poll seen/nevts mismatch: %i/%i\n",
      evts_encountered, nevts);
  }

  // delete all here.
  delete fds_copy;
  delete socs_copy;
}


// poll is the call, call the bool poll_flag
void
poll_demuxer::get_evts(bool poll_flag)
{
  // fprintf(stderr, "poll_demuxer::get_evts\n");
  void    *fds, *svs;
  int      nevts;

  get_arrays(&fds, &svs);    // we now own them. must call process_evts
                // to give them back.

  nevts = this->dopoll(fds, poll_flag);

  // don't shortcut based on nevts being zero - pass it on to process_evts
  // it recongnises the optimisation opportunity uation and handles it,
  // which has the advantage of benefiting the threadsafe version too.
  process_evts(fds, svs, nevts);
}

// precondition: not currently in get_evts (not reentrant)
int
poll_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  // fprintf(stderr, "poll::add_socket_wakeup: %p->%i, %x\n",
  //  sv, sv->s, flags);

  if(!FDS)
  {
    // fprintf(stderr, "creating fds and svns\n");
    assert(SOCS == NULL);    // both should be null or non null, no mix

    fd_array = new fdvec;    // FDS
    sv_array = new sockvec;    // SOCS
  }

  // add to array
  struct pollfd  fd;

  // note that we keep a copy of the flags, because we often have to
  // copy the wakeups back that haven't had activity
  sv->wakeup_flags = flags;

  fd.fd = sv->s;
  fd.events = 0;
  // set on output, but in the ambiguous case of a poll that returns due
  // to timeout what would it be? If I can guarantee 0, then I can use the
  // same piece of code to re-add the fds in the thread safe version.
  fd.revents = 0;

  if(flags & PDEMUX_READ) fd.events |= POLLIN;
  if(flags & PDEMUX_WRITE) fd.events |= POLLOUT;

  // don't bother setting  POLLERR or POLLHUP. They're output (revents) only.
  // is the same true for epoll_demuxer? I'd say so...

  // fd.revents is set on output by ::poll
  // fprintf(stderr, "turning all revents bits on to test 0 output\n");
  // fd.revents = -1;

  assert(0 != fd.events);

  // add to array along with sv pointer
  FDS->push_back(fd);
  SOCS->push_back(sv);

  return 0;      // there'll be a wakeup
}

}}

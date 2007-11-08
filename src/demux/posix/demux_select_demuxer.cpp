// P.S. for current impl don't need the pthreads. WHOO!!!

// A very light wrapper around select, that allows the addition
// of new sockets and returns status in a queue.
// on the powerbook with 10.3, FD_SETSIZE is 1024, that means
// max 1024 sockets. That's kind of lame. See IO completion ports
// on NT for a better solution.

// See ACE_Handle_Set_Iterator for an optimised seelect bit field examination
// algorithm (p149 C++ network programming, volume1)

// see epoll, kqueue & IOCPs

// is select level triggered? I think it is.
// strangely, I'm never seeing anything from the exception set.

#include "demux_select_demuxer.hpp"

#include <assert.h>
#include <string.h>       // memset

#include <stdio.h>        // printf debug
#include <stdlib.h>
#include "demux_sockety.hpp"  // get_socket_error
#include <memory>
#include <sys/time.h>

namespace flx { namespace demux {

select_demuxer::select_demuxer()
{
  // clear these guys. after the thread starts, access to them will have
  // to be via the lock
  FD_ZERO(&master_read_set);
  FD_ZERO(&master_write_set);
  FD_ZERO(&master_except_set);
  fdmax = 0;        // corresponds to stdin, which we're not using

  // clear this possibly quite large list
  //memset(svs, 0, sizeof(svs));
  //JS: memset must not be used except for raw data or chars
  std::uninitialized_fill_n(svs,FD_SETSIZE,(socket_wakeup*)0);
}

// one select, must not block indefinitely, so choose a timeslice
// or find a way to make it wake on command, like a dummy socket
void
select_demuxer::get_evts(bool poll)
{
  // to use select we must copy our arguments, as it changes them!
  // this code has been broken up in to pieces so that I can implement

  fd_set  read_set, write_set, except_set;

  copy_sets(read_set, write_set, except_set);

  if(select(read_set, write_set, except_set, poll))
    process_sets(read_set, write_set, except_set);
}

int
select_demuxer::add_socket_wakeup(socket_wakeup* sv, int flags)
{
  int s = sv->s;

  // fprintf(stderr, "adding select wakeup for %i, flags=%x\n", s, flags);

  if(s < 0 || s >= FD_SETSIZE) return -1; // weakness of select

  assert(svs[s] == NULL);         // sanity check: nothing there

  if(flags & PDEMUX_READ) FD_SET(s, &master_read_set);

  if(flags & PDEMUX_WRITE) FD_SET(s, &master_write_set);

  // does this mean we could add a non-reading, non-writing socket
  // and wait for errors on it?
  FD_SET(s, &master_except_set);

  svs[s] = sv;              // record wakeup. ours now.


  if(s > fdmax) fdmax = s;        // update highwater mark

  return 0;
}

// removes for both reading AND writing.
void
select_demuxer::remove_fd(int s)
{
  // fprintf(stderr, "removing select fd: %i\n", s);

  assert(s >= 0 && s < FD_SETSIZE);
  assert(svs[s] != NULL);         // there should be something there

  // clear them all regardless.
  FD_CLR(s, &master_read_set);
  FD_CLR(s, &master_write_set);
  FD_CLR(s, &master_except_set);

  svs[s] = NULL;
}

void
select_demuxer::copy_sets(fd_set& rset, fd_set& wset, fd_set& exset)
{
  rset = master_read_set;
  wset = master_write_set;
  exset = master_except_set;
}

bool
select_demuxer::select(fd_set& rset, fd_set& wset, fd_set& exset, bool poll)
{
  // this is depending on my fake socket to wakeup. perhaps use the timer
  // for now.
  struct timeval  tv, *tp = NULL;

  if(poll)
  {
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    tp = &tv;
  }

  // the return value here actually has significance
  // sometimes I have to try again, or weed out bad fds.
  //if(select(fdmax+1, &read_set, &write_set, &except_set, &tv) == -1)
// nah! wait forever. none of these things shutdown properly yet.
// it'll force the async new wakeup responsiveness
  switch(::select(fdmax+1, &rset, &wset, &exset, tp))
  {
    case 0:
      return false;   // timed out, don't process sets
    break;
    case -1:
    // not the ideal reaction. I think this is where I weed out
    // the bad socket(s). would need error set.

    // closing a socket without removing can get us here. that's pretty
    // nasty, because our data would be stale. Try not to do that. I
    // wonder if the except set would tell us when the socket was
    // closed on us? Damn, you have to clear it, else you keep getting
    // the same error.
      perror("select");
      // fall through and examine except set
    break;
  }
  return true;    // call process_sets
}

void
select_demuxer::process_sets(fd_set& rset, fd_set& wset, fd_set& exset)
{
  // since we're about to traverse the socket sets anyway, we should
  // note the highest fd seen, and make that the highwater mark.
  // that way we wouldn't be guaranteed monotonically degrading performance.

  // might be worth keeping a low water mark as well.
  // I guess this is why select sucks. On osx we can only watch
  // about 1024 sockets. That sucks too. could allocate larger sets
  // with malloc... see c++ network programming book.

  // like kqueues, this code could theoretically handle separate wakeups
  // for read and write, should I do it? not right now.
  int new_fdmax = 0;

  for(int i = 0; i <= fdmax; i++)
  {
    int   flags = 0;

    if(FD_ISSET(i, &rset)) flags |= PDEMUX_READ;

    if(FD_ISSET(i, &wset)) flags |= PDEMUX_WRITE;

    // sorta suggests that I ought to call the wakeup and pass
    // an error flag on to it.
    if(FD_ISSET(i, &exset))
    {
      // don't remove bad sockets - it's an error to close the socket
      // or deallocate the wakeup without telling the source. when
      // we get socket errors, we'd better hope that there's reading
      // or writing to be done.
      // under cygwin, closing down a socket (read, write or both)
      // causes select to wake up with an exception bit. out of cygwin
      // we only wake up. In both cases, the read bit is set so
      // just handling the stuff seems to work. not sure about write.
      // posix_demuxer::socket_recv thinks the connection's closed, but
      // it all seems to work out. Yours, Confused.

      fprintf(stderr, "select error on socket %i, flags=%x\n",
        i, flags);

      int err;
      // heh heh, this isn't great to call on the pipe that is used
      // in the self pipe trick. I don't know why it's getting an
      // err anyway.
      if(get_socket_error(i, &err) == -1)
        fprintf(stderr, "get_socket_error failed!?!\n");

      fprintf(stderr, "socket err = %i, %s\n", err, strerror(err));
      // don't remove! see below
      // remove_fd(i);
    }

    //
    if(flags)
    {
      socket_wakeup*  sv = svs[i];
      // remove before wakeup so wakeup can add itself back,
      // if necessary.
      remove_fd(i);

      sv->wakeup_flags = flags;
      sv->wakeup(*this);
    }

    // to lower high-watermark, keep track of highest seen.
    if(svs[i]) new_fdmax = i;
  }

  // fprintf(stderr, "new_fdmax=%i, fdmax=%i\n", new_fdmax, fdmax);

  fdmax = new_fdmax;      // copy it back
}

}} // flx, demux

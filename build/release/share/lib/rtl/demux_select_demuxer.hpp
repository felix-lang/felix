#ifndef __FLX_DEMUX_SELECT_DEMUXER_H__
#define __FLX_DEMUX_SELECT_DEMUXER_H__

#include "demux_posix_demuxer.hpp"
#include <sys/types.h>    // for sys/select.h on osx
#include <sys/select.h>   // for fd_set
#include <sys/time.h>     // GUSI WTF?
#include <unistd.h>       // for bsd

// Unlike the other demuxers, this one is NOT thread safe, so wait and
// add socket wakeup must only be called from the same thread.
// if you're looking for the thread safe version, try ts_select_demuxer

namespace flx { namespace demux {

class DEMUX_EXTERN select_demuxer : public posix_demuxer {
  void  remove_fd(int s);

  // thanks Beej!
  fd_set      master_read_set;    // fd watched for reading
  fd_set      master_write_set;   // for writing
  fd_set      master_except_set;    // for exceptions

  // read sveglias - note we only have one set, so currently this demuxer
  // cannot have separate wakeups for the same file descriptor. this
  // fits in fine with the "undefined" nature of doing that.
  socket_wakeup*  svs[FD_SETSIZE];    // read sveglias
  //socket_wakeup*  write_svs[FD_SETSIZE];  // write wakeups

  int       fdmax;          // high watermark for select

protected:
  virtual void  get_evts(bool poll);

public:
  // get_evts broken into pieces for thread safe implementations
  void copy_sets(fd_set& rset, fd_set& wset, fd_set& exset);
  // returns true if process_sets should be called.
  bool select(fd_set& rset, fd_set& wset, fd_set& exset, bool poll);
  // these could be consts
  void process_sets(fd_set& rset, fd_set& wset, fd_set& exset);

  select_demuxer();

  virtual int   add_socket_wakeup(socket_wakeup* sv, int flags);
};
}} // namespace demux, flx
#endif

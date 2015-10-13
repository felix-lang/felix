#ifndef __FLX_DEMUX_PFILEIO_H__
#define __FLX_DEMUX_PFILEIO_H__
#include <flx_demux_config.hpp>

#include "demux_demuxer.hpp"
#include "pthread_bound_queue.hpp"
// #include <sys/types.h> // off_t (don't have flx iface to this yet)
              // can just add new constructor
#include "pthread_work_fifo.hpp"
namespace flx { namespace demux {

// ********************************************************
/// like another event source. this is basically a wrapped pread, pwrite
/// should probably be derived from posix_wakeup or something like that.
/// or have the same signature. abstract - users overload "finished
// ********************************************************
class DEMUX_EXTERN fileio_request : public flx::pthread::worker_task
{
  long    offset;   // make this a proper offset (64bit)
  // off_t    offset; // in: offset, for use with pread, pwrite
  int     fd;     // in: fd in question
  bool    read_flag;  // in: read else write

  int     err;    // out:
public:
  // public so it can be got in felix
  sel_param pb;   // in & out: what you want, what you get (64bit len?)

  virtual ~fileio_request(); // c++ should do this automatically
  fileio_request();       // flx linkage
  fileio_request(int f, char* buf, long len, long off, bool rd);

  virtual void doit();      // sync
};

}} // namespace demux, flx
#endif  // __PFILEIO__

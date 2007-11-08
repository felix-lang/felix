#ifndef __FLX_DEMUX_WSELF_PIPER_H__
#define __FLX_DEMUX_WSELF_PIPER_H__

#include <flx_demux_config.hpp>
#include "demux_overlapped.hpp"  // I use readfile control block
#include "demux_wself_piper.hpp"

namespace flx { namespace demux {

class DEMUX_EXTERN auto_handle {
public:
  HANDLE h;

  auto_handle();
  ~auto_handle();
};

// win32 self pipe trick (what a pain!)
class DEMUX_EXTERN wpipe_pair {
  enum { READ_END, WRITE_END };
  // HANDLE pipe[2];  // 0 = read end, 1 = write end
  auto_handle pipe[2];
public:
  wpipe_pair();

  void write_byte();
  HANDLE get_read_end() { return pipe[READ_END].h; }
};

// use a winfileio_control_block to install a nonblocking ReadFile on the
// read end of the pipe pair. when we get a byte we can execute whatever
// the user wanted which for win32 which seems to be naturally responsive
// to new sockets/handles. demux quit will probably be the only operation
// needed.
class DEMUX_EXTERN wself_piper_wakeup : public winfileio_control_block
{
  char the_byte;

public:
  // possibly null, if not, called on iocp_op_finished
  demux_callback* cb;

  // the demuxer. doesn't actually get passed by iocp_op_finished
  iocp_demuxer* d;


  wself_piper_wakeup();

  // detect when single byte read has finished and exec callback,
  // re-arming.
  virtual void iocp_op_finished(DWORD nbytes, ULONG_PTR udat,
    LPOVERLAPPED olp, int err);

  void arm();
};

// at the very least, the read end must be nonblocking and associated
// with the iocp.
class DEMUX_EXTERN wself_piper {
  wpipe_pair pp;
  wself_piper_wakeup spw;
public:
  void install(demuxer* demux, demux_callback* cb = 0);
  void wake(); // wakes demuxer which calls callback
};

}} // demux, flx

#endif

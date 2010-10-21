#ifndef __FLX_DEMUX_DEMUXER_H__
#define __FLX_DEMUX_DEMUXER_H__
#include <flx_demux_config.hpp>

namespace flx { namespace demux {

struct sel_param {
  char*   buffer;           // set on input
  long    buffer_size;      // set on input
  long    bytes_written;    // set on input and output
  bool    eof_detected;     // NEW: a proper eof flag
                            // maybe implemented in Posix, 
                            // not done in Windows yet (since I don't have a Windows development box)

  bool    error_detected;   // NEW: a proper error flag
                            // maybe implemented on Posix
                            // not done in Windows yet (since I don't have a Windows development box)
                            // NOTE: there's no indication of what the error is
                            // because that is platform dependent
                            // A normal EOF condition is NOT an error
                            // However most errors will be associated with an eof!

  sel_param() : buffer(0), buffer_size(0), bytes_written(0), eof_detected(false), error_detected(false)  {}
  bool    finished() { return error_detected || eof_detected || (bytes_written == buffer_size); }
};

// rename ...
// read/write flags - they're no longer mutually exclusive
enum { PDEMUX_READ = 1, PDEMUX_WRITE = 2, PDEMUX_EOF=4, PDEMUX_ERROR=8 };

// base class/hook for implementing thread safe multithreaded demux quit
// not that useful for single threaded implementations.
class DEMUX_EXTERN demux_quit_flag
{
public:
  virtual void signal_true() = 0; // = signal finish
  virtual ~demux_quit_flag() {}
};

// ********************************************************
/// Demux base.
// ********************************************************
class DEMUX_EXTERN demuxer {
protected:
  // wait for outstanding events. may return before given events, so
  // check your conditions. I've turned of all the timeouts that cause
  // this, but don't rely on it!
  // FACTOR. Give poll a greedy interface
  virtual void  get_evts(bool poll) = 0;

  // for clean async takedown. contents guaranteed to be valid until
  // quit_flag->signal_true is called
  demux_quit_flag* quit_flag;
public:
  demuxer() : quit_flag(0) {}
  virtual ~demuxer() {}

  void wait() { get_evts(false); }
  void poll() { get_evts(true); }

  // ask users of demuxer to exit. not guarded. be sure to either set & get
  // this flag from only one thread (with a wait/wakeup callback - see
  // self_piper) or by using a memory barrier.
  virtual demux_quit_flag* get_quit_flag() { return quit_flag; }
  virtual void set_quit_flag(demux_quit_flag* f) { quit_flag = f; }
};

// base class for callback from demuxer. useful when used in conjuction
// with the self piper for implementing threadsafe demuxer quit and
// guaranteeing responsiveness to new sockets.
// run in the same thread that called d->wait/poll.
class DEMUX_EXTERN demux_callback {
public:
  virtual void callback(demuxer* d) = 0;
  virtual ~demux_callback() {}
};

}} // namespace demux, flx
#endif  /* __DEMUXER__ */

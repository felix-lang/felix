#ifndef __FLX_DEMUX_POLL_DEMUXER_H__
#define __FLX_DEMUX_POLL_DEMUXER_H__

#include <flx_demux_config.hpp>
#include "demux_posix_demuxer.hpp"

// not re-entrant

namespace flx { namespace demux {

class DEMUX_EXTERN poll_demuxer : public posix_demuxer {
  void*  fd_array;    // make him stop!
  void*  sv_array;

  virtual void get_evts(bool poll);
public:
  poll_demuxer();
  virtual ~poll_demuxer();

  virtual int add_socket_wakeup(socket_wakeup* sv, int flags);

  void get_arrays(void** fds, void** svs);
  int dopoll(void* infds, bool poll_flag);    // returns nevents
  void process_evts(void* infds, void* svs, int nevts);
};

}}
#endif

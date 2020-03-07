#ifndef __FLX_DEMUX_EVTPORT_DEMUXER_H__
#define __FLX_DEMUX_EVTPORT_DEMUXER_H__

// driver for solaris 10 event port notifications

#include "demux_posix_demuxer.hpp"

namespace flx { namespace demux {

// Event ports are oneshot by default (I don't know if you can change that).
// Events are tracked only by fd and not fd*event, so you cannot add
// separate wakeups for read and write with the same fd and hope for it to
// work as the later one will overwrite the earlier, fodder for race
// conditions. This impl satisfies 1-1 wakeup to request ratio.

// I don't know if evtports can be waited upon by other evtports

// OBS.
// after removing the threads from the demuxers/event sources
// how are the two half demuxers supposed to work? They used to
// have three threads and now they have one. How can two waits be
// done in one thread? I could add one half_demuxer's evtport to
// the other's and wait on that. Would that work? Otherwise I'll
// have to start a thread, which screws things up a bit. Could do
// that and communicate back to single thread via a waitable queue.
// could have three half-demuxers, add them both to third and call
// their wait functions depending on the outer's wait result.

class DEMUX_EXTERN evtport_demuxer : public posix_demuxer {
    int     evtport;

  // I think evtports only track socket the socket and not
  // socket*operation, so there's only one remove
  void remove_wakeup(int s);

    virtual void  get_evts(bool poll);
public:
  evtport_demuxer();
  virtual ~evtport_demuxer();

  virtual int   add_socket_wakeup(socket_wakeup* sv, int flags);
};

}} // namespace demux, flx
#endif

#ifndef __FLX_DEMUX_H__
#define __FLX_DEMUX_H__
#include <flx_demux_config.hpp>
#include "pthread_thread.hpp"

#if FLX_WIN32
 #include "demux_iocp_demuxer.hpp"
 namespace flx { namespace demux {
   typedef iocp_demuxer flx_demuxer_t;
 }}
#elif FLX_HAVE_KQUEUE_DEMUXER
 #include "demux_kqueue_demuxer.hpp"
  namespace flx { namespace demux {
    typedef kqueue_demuxer flx_demuxer_t;
  }}
#elif FLX_HAVE_EVTPORTS
 #include "demux_evtport_demuxer.hpp"
  namespace flx { namespace demux {
    typedef evtport_demuxer flx_demuxer_t;
  }}
#elif FLX_HAVE_EPOLL
 #include "demux_epoll_demuxer.hpp"
  namespace flx { namespace demux {
    typedef epoll_demuxer flx_demuxer_t;
  }}
#elif FLX_HAVE_POLL
 // NB!: on osx 10.3 poll exists, but is a poor cousin emulation layer on
 // top of select. however, 10.3 has kqueues (above), so should be ok...
 #include "demux_ts_poll_demuxer.hpp"
  namespace flx { namespace demux {
    typedef ts_poll_demuxer flx_demuxer_t;
  }}
#else
 #include "demux_ts_select_demuxer.hpp"
  namespace flx { namespace demux {
    typedef ts_select_demuxer flx_demuxer_t;
  }}
#endif


namespace flx { namespace demux {
DEMUX_EXTERN flx_demuxer_t * make_std_demuxer();
}}

#endif

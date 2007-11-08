#ifndef __FLX_FAIO_DRV_H__
#define __FLX_FAIO_DRV_H__
#include <flx_faio_config.hpp>

#include "pthread_sleep_queue.hpp"
#include "pthread_work_fifo.hpp"
#include "demux_timer_queue.hpp"
#include "demux_demuxer.hpp"

namespace flx { namespace faio {

// this may be needed but I've lost track of where
// we get SIGPIPE, SIG_IGN from ..

#if 0
void FAIO_EXTERN sigpipe_ignorer()
{
    void (*prev_handler)(int);  // solaris is FUN.
    prev_handler = signal(SIGPIPE, SIG_IGN);

    if(SIG_ERR == prev_handler)
    {
        fprintf(stderr, "failed to install SIGPIPE ignorer\n");
        throw -1;
    }
    else if(prev_handler != SIG_IGN && prev_handler != SIG_DFL)
    {
        fprintf(stderr,"warning: blew away prev SIGPIPE handler: %p\n",
            prev_handler);
    }
}
#endif

}}
#endif

#include "flx_demux.hpp"
#include <stdio.h>
#include <stdlib.h>

namespace flx { namespace demux {

// the thread which runs the demuxer polling loop
static void
pthread_thread(void* udat)
{
    demuxer*    d = (demuxer*)udat;

    while(1)
    {
        //fprintf(stderr, "ETHREAD ABOUT TO WAIT\n");
        d->wait();          // this does it
        //fprintf(stderr, "ETHREAD CHECKING QUIT FLAG\n");
        demux_quit_flag* f = d->get_quit_flag();
        if(f)
        {
          // got a quit flag - this is the very last thing we do before
          // exiting. don't use the demuxer after this as it's probably been
          // destructed.
          fprintf(stderr, "ETHREAD GOT QUIT FLAG, SIGNALLING AND EXITING\n");
          f->signal_true();
          // in the case of a system takedown there's no guarantee that
          // anything after the signal_finish will be run at all, so this
          // is not a good place to put anything important.
          break;  // outta here
        }
    }
    fprintf(stderr, "ETHREAD EXITING\n");
    fprintf(stderr, "proto_async was asked to quit...\n");
}

flx_demuxer_t *
make_std_demuxer()
{
  flx_demuxer_t *d = new flx_demuxer_t();
  pthread::flx_thread_t ethread;
  if(ethread.init(pthread_thread, d, NULL) == -1)
  {
    fprintf(stderr,"Proto_async thread init failure\n");
    exit(1);
  }
  return d;
}
}}

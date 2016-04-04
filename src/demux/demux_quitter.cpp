#include "demux_quitter.hpp"
#include <stdio.h>

namespace flx { namespace demux {

// this is called by the demuxer, from its event thread.
// for this reason it can modify the demuxer with tranquility, asking it
// to quit, by setting the "quit flag". this is an obj with a virtual
// signal_true method which the event thread/demuxer user calls, in effect
// promising to never interact with that demuxer ever again. what to do with
// the wakeups that demuxer still knows about is unclear, and with most
// demuxer impls there isn't even a way to know which wakeups are outstanding.
// it hasn't been a problem yet.
void
demux_quitter::callback(demuxer* demux)
{
  fprintf(stderr, "demux quitter callback, setting quit flag from event thread\n");
  demux->set_quit_flag(this);
}

// call this last thing before event thread exit, or more explicitly
// call it before never touching the given demuxer ever again. see?
// it is linked to demuxers. a better name for this method would be
// i_promise_to_never_use_the_associated_object_ever_again_clear_question_mark
void
demux_quitter::signal_true() // signal finish, from demux_quit_flag
{
fprintf(stderr,"Demux quiter signal_tru()\n");
  finished.signal_true();
  // do NOTHING here, we've probably already been destructed, mr anderson.
  // I told you, my name is "neil".
}

void
demux_quitter::quit(demuxer* demux)
{
   fprintf(stderr, "demux_quitter::quit() trying to quit demuxer...\n");
   // install self piper, with our callback
   sp.install(demux, this);
   // wake demuxer, getting our callback called, which sets quit flag
   // NB: this requires that there BE another thread.
   sp.wake();
   // wait for quit flag to be signalled by exiting event thread
   fprintf(stderr, "demux_quitter::quit()  waiting for demuxer to quit\n");
   finished.wait_until_true();
   fprintf(stderr, "demux_quitter::quit()  demuxer has quit\n");
   // event thread exited
}

}}

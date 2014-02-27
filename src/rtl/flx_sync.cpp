#include <stdio.h>

#include "flx_sync.hpp"

using namespace flx::rtl;

namespace flx { namespace run {

RTL_EXTERN char const *get_fstate_desc(fstate_t fs)
{
  switch(fs)
  {
    case terminated: return "terminated";
    case blocked: return "blocked";
    case delegated: return "delegated";
    default: return "Illegal fstate_t";
  }
}

RTL_EXTERN char const *get_fpc_desc(fpc_t fpc)
{
  switch(fpc)
  {
    case next_fthread_pos: return "Next fthread pos";
    case next_request_pos: return "Next request pos";
    default: return "Illegal fpc_t";
  }
}


sync_state_t::sync_state_t (
  bool debug_driver_,
  flx::gc::generic::gc_profile_t *gcp,
  std::list<fthread_t*> *active_
) :
  debug_driver(debug_driver_),
  collector(gcp->collector),
  active(active_),
  pc(next_fthread_pos)
{}

void sync_state_t::do_yield()
    {
      if(debug_driver)fprintf(stderr,"yield");
      active->push_back(ft);
      pc=next_fthread_pos;
    }

void sync_state_t::do_spawn_detached()
    {
      fthread_t *ftx = *(fthread_t**)request->data;
      if(debug_driver)fprintf(stderr,"Spawn thread %p\n",ftx);
      collector->add_root(ftx);
      active->push_front(ftx);
      pc=next_request_pos;
    }

void sync_state_t::do_sread()
    {
      readreq_t * pr = (readreq_t*)request->data;
      schannel_t *chan = pr->chan;
      if(debug_driver)fprintf(stderr,"Request to read on channel %p\n",chan);
      if(chan==NULL) goto svc_read_none;
    svc_read_next:
      {
        fthread_t *writer= chan->pop_writer();
        if(writer == 0) goto svc_read_none;       // no writers
        if(writer->cc == 0) goto svc_read_next;   // killed
        readreq_t * pw = (readreq_t*)writer->get_svc()->data;
        if(debug_driver)fprintf(stderr,"Writer @%p=%p, read into %p\n", pw->variable,*(void**)pw->variable, pr->variable);
        *(void**)pr->variable = *(void**)pw->variable;
        active->push_front(writer);
        collector->add_root(writer);
         pc=next_request_pos; return;
      }

    svc_read_none:
      if(debug_driver)fprintf(stderr,"No writers on channel %p: BLOCKING\n",chan);
      chan->push_reader(ft);
      // forget_fthread
      if(debug_driver)fprintf(stderr,"unrooting fthread %p\n",ft);
      collector->remove_root(ft);
      pc=next_fthread_pos; return;
    }

void sync_state_t::do_swrite()
    {
      readreq_t * pw = (readreq_t*)request->data;
      schannel_t *chan = pw->chan;
      if(debug_driver)fprintf(stderr,"Request to write on channel %p\n",chan);
      if(chan==NULL)goto svc_write_none;
    svc_write_next:
      {
        fthread_t *reader= chan->pop_reader();
        if(reader == 0) goto svc_write_none;     // no readers
        if(reader->cc == 0) goto svc_write_next; // killed
        readreq_t * pr = (readreq_t*)reader->get_svc()->data;
        if(debug_driver)fprintf(stderr,"Writer @%p=%p, read into %p\n", pw->variable,*(void**)pw->variable, pr->variable);
        *(void**)pr->variable = *(void**)pw->variable;
        // NEW: ESSENTIAL! Reader must continue on, not writer!
        active->push_front(ft);
        collector->add_root(reader);
        ft=reader;
        pc=next_request_pos; return;
      }
    svc_write_none:
      if(debug_driver)fprintf(stderr,"No readers on channel %p: BLOCKING\n",chan);
      chan->push_writer(ft);
      // forget_fthread
      if(debug_driver)fprintf(stderr,"unrooting fthread %p\n",ft);
      collector->remove_root(ft);
      pc=next_fthread_pos; return;
    }

void sync_state_t::do_multi_swrite()
    {
      readreq_t * pw = (readreq_t*)request->data;
      schannel_t *chan = pw->chan;
      if(debug_driver)fprintf(stderr,"Request to write on channel %p\n",chan);
      if(chan==NULL) { pc=next_request_pos; return; }
    svc_multi_write_next:
      fthread_t *reader= chan->pop_reader();
      if(reader == 0)  { pc=next_request_pos; return; }    // no readers left
      if(reader->cc == 0) goto svc_multi_write_next; // killed
      {
        readreq_t * pr = (readreq_t*)reader->get_svc()->data;
        if(debug_driver)fprintf(stderr,"Writer @%p=%p, read into %p\n", pw->variable,*(void**)pw->variable, pr->variable);
        *(void**)pr->variable = *(void**)pw->variable;
        // NEW: ESSENTIAL! Reader must continue on, not writer!
        active->push_front(ft);
        collector->add_root(reader);
        ft=reader;
      }
      goto svc_multi_write_next;
    }

void sync_state_t::do_kill()
    {
      fthread_t *ftx = *(fthread_t**)request->data;
      if(debug_driver)fprintf(stderr,"Request to kill fthread %p\n",ftx);
      ftx -> kill();
      pc=next_request_pos; return;
    }


void sync_state_t::frun()
{
dispatch:
  // dispatch
  if (pc == next_request_pos) goto next_request;
  if (pc == next_fthread_pos) goto next_fthread;
  fprintf(stderr,"BUG -- unreachable code in frun\n");
  abort();

next_fthread:
  if (active->size() == 0) {
    fs = blocked;
    pc = next_fthread_pos;
    return;
  }
  ft = active->front();
  active->pop_front();

next_request:
  request = ft->run();
  if(request != 0) goto check_collect;

forget_fthread:
  if(debug_driver)fprintf(stderr,"unrooting fthread %p\n",ft);
  collector->remove_root(ft);
  goto next_fthread;

delegate:
  pc = next_request_pos;
  fs = delegated;
  return;

check_collect:
  switch(request->variant)
  {
    case svc_yield: do_yield(); goto dispatch;

    case svc_spawn_detached: do_spawn_detached(); goto dispatch;

    case svc_sread: do_sread(); goto dispatch;

    case svc_swrite: do_swrite(); goto dispatch;

    case svc_multi_swrite: do_multi_swrite(); goto dispatch;

    case svc_kill: do_kill(); goto dispatch;

    default:  goto delegate;
  }
  fprintf(stderr,"BUG unreachable code executed\n");
  abort();
}

}}

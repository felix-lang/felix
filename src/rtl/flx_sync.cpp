#include <stdio.h>

#include "flx_sync.hpp"

using namespace flx::rtl;

namespace flx { namespace run {

char const *sync_sched::get_fstate_desc()
{
  switch(fs)
  {
    case blocked: return "blocked";
    case delegated: return "delegated";
    default: return "Illegal fstate_t";
  }
}

char const *sync_sched::get_fpc_desc()
{
  if (ft)
    return "Next request pos";
  else
  {
    if (active->size() > 0) return "Next fthread pos";
    else return "Out of active threads";
  }
}


sync_sched::sync_sched (
  bool debug_driver_,
  ::flx::gc::generic::gc_profile_t *gcp_,
  ::std::list<fthread_t*> *active_
) :
  debug_driver(debug_driver_),
  collector(gcp_->collector),
  active(active_),
  ft(0)
{}

void sync_sched::forget_current()
  {
     if(ft) 
     {
       collector->remove_root(ft);
       pop_current();
    }
  }

void sync_sched::pop_current()
  {
     if(active->size() > 0) 
     {
       ft = active->front();
       active->pop_front();
     }
     else
       ft = 0;
  }

void sync_sched::push_new(fthread_t *f)
  {
    collector->add_root(f);
    push_old(f);
  }

void sync_sched::push_old(fthread_t *f)
  {
    if(ft) active->push_front(ft);
    ft = f;
  }

void sync_sched::do_yield()
    {
      if(debug_driver)
         fprintf(stderr,"[sync: svc_yield] yield");
      active->push_back(ft);
      pop_current();
    }

void sync_sched::do_spawn_detached()
    {
      fthread_t *ftx = *(fthread_t**)request->data;
      if(debug_driver)
        fprintf(stderr,"[sync: svc_spawn_detached] Spawn fthread %p\n",ftx);
      push_new(ftx);
    }

void sync_sched::do_schedule_detached()
    {
      fthread_t *ftx = *(fthread_t**)request->data;
      if(debug_driver)
        fprintf(stderr,"[sync: svc_schedule_detached] Schedule fthread %p\n",ftx);
      collector->add_root(ftx);
      active->push_back(ftx);
    }

void sync_sched::do_sread()
    {
      readreq_t * pr = (readreq_t*)request->data;
      schannel_t *chan = pr->chan;
      if(debug_driver)
        fprintf(stderr,"[sync: svc_read] Request to read on channel %p\n",chan);
      if(chan==NULL) goto svc_read_none;
    svc_read_next:
      {
        fthread_t *writer= chan->pop_writer();
        if(writer == 0) goto svc_read_none;       // no writers
        if(writer->cc == 0) goto svc_read_next;   // killed
        readreq_t * pw = (readreq_t*)writer->get_svc()->data;
        if(debug_driver)
          fprintf(stderr,"[sync: svc_read] Writer @%p=%p, read into %p\n", 
            pw->variable,*(void**)pw->variable, pr->variable);
        *(void**)pr->variable = *(void**)pw->variable;
        // reader goes first!
        active->push_front(writer);
        collector->add_root(writer);
        return;
      }

    svc_read_none:
      if(debug_driver)
        fprintf(stderr,"[sync: svc_read] No writers on channel %p: BLOCKING\n",chan);
      chan->push_reader(ft);
      forget_current();
      return;
    }

void sync_sched::do_swrite()
    {
      readreq_t * pw = (readreq_t*)request->data;
      schannel_t *chan = pw->chan;
      if(debug_driver)
         fprintf(stderr,"[sync: svc_write] Request to write on channel %p\n",chan);
      if(chan==NULL)goto svc_write_none;
    svc_write_next:
      {
        fthread_t *reader= chan->pop_reader();
        if(reader == 0) goto svc_write_none;     // no readers
        if(reader->cc == 0) goto svc_write_next; // killed
        readreq_t * pr = (readreq_t*)reader->get_svc()->data;
        if(debug_driver)
          fprintf(stderr,"[sync: svc_write] Writer @%p=%p, read into %p\n", 
            pw->variable,*(void**)pw->variable, pr->variable);
        *(void**)pr->variable = *(void**)pw->variable;
        push_new (reader);
        return;
      }
    svc_write_none:
      if(debug_driver)
        fprintf(stderr,"[sync: svc_write] No readers on channel %p: BLOCKING\n",chan);
      chan->push_writer(ft);
      forget_current();
      return;
    }

void sync_sched::external_multi_swrite (schannel_t *chan, void *data)
    {
      if(chan==NULL) return;
    svc_multi_write_next:
      fthread_t *reader= chan->pop_reader();
      if(reader == 0)  return;    // no readers left
      if(reader->cc == 0) goto svc_multi_write_next; // killed
      {
        readreq_t * pr = (readreq_t*)reader->get_svc()->data;
        if(debug_driver)
           fprintf(stderr,"[sync: svc_multi_write] Write data %p, read into %p\n", 
             data, pr->variable);
        *(void**)pr->variable = data;
        push_new(reader);
      }
      goto svc_multi_write_next;
    }

void sync_sched::do_multi_swrite()
    {
      readreq_t * pw = (readreq_t*)request->data;
      void *data = *(void**)pw->variable;
      schannel_t *chan = pw->chan;
      if(debug_driver)
        fprintf(stderr,"[sync: svc_multi_write] Request to write on channel %p\n",chan);
      external_multi_swrite (chan, data);
    }

void sync_sched::do_kill()
    {
      fthread_t *ftx = *(fthread_t**)request->data;
      if(debug_driver)fprintf(stderr,"[sync: svc_kill] Request to kill fthread %p\n",ftx);
      ftx -> kill();
      return;
    }


void sync_sched::frun()
{
  if (debug_driver)
     fprintf(stderr,"[sync] frun: entry ft=%p, active size=%ld\n", ft,active->size());
dispatch:
  if (ft == 0) 
  {
    if (active->size() == 0)  // out of active fthreads
    {
      fs = blocked;
      return;
    }
    ft = active->front();     // grab next fthread
    active->pop_front();
  }
  request = ft->run();        // run fthread to get request
  if(request == 0)            // euthenasia request
  {
    if(debug_driver)
      fprintf(stderr,"unrooting fthread %p\n",ft);
    collector->remove_root(ft);
    ft = 0;
    goto dispatch;
  }

  switch(request->variant)
  {
    case svc_yield: do_yield(); goto dispatch;

    case svc_spawn_detached: do_spawn_detached(); goto dispatch;

    case svc_sread: do_sread(); goto dispatch;

    case svc_swrite: do_swrite(); goto dispatch;

    case svc_multi_swrite: do_multi_swrite(); goto dispatch;

    case svc_kill: do_kill(); goto dispatch;

    default:  
      fs = delegated;
      return;
  }
}

}}

#include "flx_rtl.hpp"
#include "flx_rtl_shapes.hpp"

#include <cstdio>
#include <cassert>
#include <cstddef>
#include <stdint.h>
#include "flx_exceptions.hpp"
#include "flx_collector.hpp"
#include "flx_serialisers.hpp"

// main run time library code

namespace flx { namespace rtl {

// ********************************************************
// con_t implementation
// ********************************************************

con_t::con_t() : pc(0), p_svc(0), _caller(0) {
#if FLX_DEBUG_CONT
 fprintf(stderr,"Constructing %p\n",this);
#endif
}
con_t::~con_t(){
#if FLX_DEBUG_CONT
  fprintf(stderr,"Destroying %p\n",this);
#endif
}

// ********************************************************
// slist implementation
// ********************************************************

slist_t::slist_t(::flx::gc::generic::gc_profile_t *_gcp) : gcp (_gcp), head(0) {}
slist_t::slist_t(slist_t const &r) : gcp (r.gcp), head(r.head) {}

bool slist_t::isempty()const { return head == 0; }

void slist_t::push(void *data)
{
  head = new(*gcp,slist_node_ptr_map,true) slist_node_t(head,data);
}

// note: never fails, return NULL pointer if the list is empty
void *slist_t::pop()
{
  if(head) {
    void *data = head->data;
    head=head->next;
    return data;
  }
  else return 0;
}
// ********************************************************
// fthread_t implementation
// ********************************************************

fthread_t::fthread_t() : cc(0) {}
fthread_t::fthread_t(con_t *a) : cc(a) {}

// uncopyable object but implementation needed for linker
fthread_t::fthread_t(fthread_t const&){ assert(false); }
void fthread_t::operator=(fthread_t const&){ assert(false); }

void fthread_t::kill() { cc = 0; }
//
// ********************************************************
// _uctor_t implementation
// ********************************************************

_uctor_ *fthread_t::get_svc()const { return cc?cc->p_svc:0; }

_uctor_ *fthread_t::run() {
  if(!cc) return 0; // dead
restep:
  cc->p_svc = 0;
step:
  //fprintf(stderr,"cc=%p->",cc);
  try { cc = cc->resume(); }
  catch (con_t *x) { cc = x; }

  //fprintf(stderr,"->%p\n",cc);
  if(!cc) return 0; // died

  if(cc->p_svc)
  {
    switch(cc->p_svc->variant)
    {
      case svc_get_fthread:
        // NEW VARIANT LAYOUT RULES
        // One less level of indirection here
        //**(fthread_t***)(cc->p_svc->data) = this;
        *(fthread_t**)(cc->p_svc->data) = this;
        goto restep;      // handled

      //case svc_yield:
      //  goto restep;

      // we don't know what to do with the request,
      // so pass the buck to the driver
      default:
        return cc->p_svc;
    }
  }
  goto step;
}

// ********************************************************
// schannel_t implementation
// ********************************************************

schannel_t::schannel_t (gc::generic::gc_profile_t *gcp) :
  waiting_to_read(0), waiting_to_write(0)
{
  waiting_to_read = new (*gcp, slist_ptr_map,false) slist_t(gcp);
  waiting_to_write = new (*gcp, slist_ptr_map,false) slist_t(gcp);
}

// uncopyable object but implementation needed for linker
schannel_t::schannel_t(schannel_t const&) { assert(false); }
void schannel_t::operator=(schannel_t const&) { assert(false); }

void schannel_t::push_reader(fthread_t *r)
{
  waiting_to_read->push(r);
}

void schannel_t::push_writer(fthread_t *w)
{
  waiting_to_write->push(w);
}

fthread_t *schannel_t::pop_reader()
{
  return (fthread_t*)waiting_to_read->pop();
}

fthread_t *schannel_t::pop_writer()
{
  return (fthread_t*)waiting_to_write->pop();
}
// ********************************************************
// trace feature
// ********************************************************

int flx_enable_trace=1;
unsigned long flx_global_trace_count=0uL;

void flx_trace(flx_trace_t* tr,flx_range_srcref_t sr, char const *file, int line, char const *msg)
{
  if(!flx_enable_trace)return;
  flx_global_trace_count++;
  if(tr)
  {
    tr->count++;
    if(tr->enable_trace)
    {
      fprintf(stderr,"%ld : %s\n",tr->count,msg);
      print_loc(stderr,sr,file,line);
    }
  }
  else
  {
    fprintf(stderr,"%ld : %s\n",flx_global_trace_count,msg);
    print_loc(stderr,sr,file,line);
  }
}


}}

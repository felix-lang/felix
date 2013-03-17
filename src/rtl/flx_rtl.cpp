#include "flx_rtl.hpp"
#include <cstdio>
#include <cassert>
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

// PRIVATE to the slist_t class!
struct slist_node_t {
  slist_node_t *next;
  void *data;
  slist_node_t(slist_node_t *n, void *d) : next(n), data(d) {}
};

//OFFSETS for slist_node_t
static const std::size_t slist_node_offsets[2]={
    offsetof(slist_node_t,next),
    offsetof(slist_node_t,data)
};

static ::flx::gc::collector::offset_data_t const slist_node_offset_data = { 2, slist_node_offsets };
::flx::gc::generic::gc_shape_t const slist_node_ptr_map = {
  NULL,
  "slist_node_t",
  1,sizeof(slist_node_t),
  0, // no finaliser,
  &slist_node_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<slist_node_t>,::flx::gc::generic::tunblit<slist_node_t>, 
  ::flx::gc::generic::gc_flags_default
};


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

//OFFSETS for slist_t
static const std::size_t slist_offsets[1]={
    offsetof(slist_t,head)
};
static ::flx::gc::collector::offset_data_t const slist_offset_data = { 1, slist_offsets };

::flx::gc::generic::gc_shape_t const slist_ptr_map = {
  &slist_node_ptr_map,
  "slist_t",
  1,sizeof(slist_t),
  0, // no finaliser
  &slist_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<slist_t>,::flx::gc::generic::tunblit<slist_t>, 
  ::flx::gc::generic::gc_flags_default
};

// ********************************************************
// fthread_t implementation
// ********************************************************

fthread_t::fthread_t() : cc(0) {}
fthread_t::fthread_t(con_t *a) : cc(a) {}

// uncopyable object but implementation needed for linker
fthread_t::fthread_t(fthread_t const&){ assert(false); }
void fthread_t::operator=(fthread_t const&){ assert(false); }

void fthread_t::kill() { cc = 0; }

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


//OFFSETS for fthread_t
static const std::size_t _fthread_offsets[1]={
    offsetof(fthread_t,cc)
};

static ::flx::gc::collector::offset_data_t const _fthread_offset_data = { 1, _fthread_offsets };

::flx::gc::generic::gc_shape_t const _fthread_ptr_map = {
  &slist_ptr_map,
  "fthread_t",
  1,sizeof(fthread_t),
  0,
  &_fthread_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<fthread_t>,::flx::gc::generic::tunblit<fthread_t>, 
  gc::generic::gc_flags_immobile
};

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

//OFFSETS for schannel_t
static const std::size_t schannel_offsets[2]={
    offsetof(schannel_t,waiting_to_read),
    offsetof(schannel_t,waiting_to_write)
};

static ::flx::gc::collector::offset_data_t const schannel_offset_data = { 2, schannel_offsets };

::flx::gc::generic::gc_shape_t const schannel_ptr_map = {
  &_fthread_ptr_map,
  "schannel_t",
  1,sizeof(schannel_t),
  0, // no finaliser
  &schannel_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<schannel_t>,::flx::gc::generic::tunblit<schannel_t>, 
  gc::generic::gc_flags_default
};

// ********************************************************
// _uctor_ implementation
// ********************************************************

//OFFSETS for _uctor_
static const std::size_t _uctor_offsets[1]= {
  offsetof(_uctor_,data)
};

static ::flx::gc::collector::offset_data_t const _uctor_offset_data = { 1, _uctor_offsets };

::flx::gc::generic::gc_shape_t const _uctor_ptr_map = {
  &schannel_ptr_map,
  "_uctor_",
  1,
  sizeof(_uctor_),
  0,
  &_uctor_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<_uctor_>,::flx::gc::generic::tunblit<_uctor_>, 
  gc::generic::gc_flags_default
};

// ********************************************************
// int implementation
// ********************************************************


::flx::gc::generic::gc_shape_t const _int_ptr_map = {
  &_uctor_ptr_map,
  "int",
  1,
  sizeof(int),
  0,
  0,
  0,
  ::flx::gc::generic::tblit<int>,::flx::gc::generic::tunblit<int>, 
  gc::generic::gc_flags_default
};

// ********************************************************
// pointer implementation
// ********************************************************

//OFFSETS for address
static const std::size_t _address_offsets[1]={ 0 };
static ::flx::gc::collector::offset_data_t const _address_offset_data = { 1, _address_offsets };

static ::std::string address_encoder (void *p) { 
  return ::flx::gc::generic::blit (p,sizeof (void*));
}

static size_t address_decoder (void *p, char *s, size_t i) { 
  return ::flx::gc::generic::unblit (p,sizeof (void*),s,i);
}


::flx::gc::generic::gc_shape_t const _address_ptr_map = {
  &_int_ptr_map,
  "address",
  1,
  sizeof(void*),
  0,
  &_address_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<void*>,::flx::gc::generic::tunblit<void*>, 
  gc::generic::gc_flags_default
};


// ********************************************************
// unit implementation : MUST BE LAST because the compiler
// uses "unit_pre_map" as the back link for generated shape tables
// ********************************************************

::flx::gc::generic::gc_shape_t const unit_ptr_map = {
  &_address_ptr_map,
  "unit",
  1,
  sizeof(unit),
  0,
  0,
  0,
  ::flx::gc::generic::tblit<unit>,::flx::gc::generic::tunblit<unit>, 
  gc::generic::gc_flags_default
};

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

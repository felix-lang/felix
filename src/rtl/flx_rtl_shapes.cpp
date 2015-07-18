#include "flx_rtl_shapes.hpp"
#include "flx_rtl.hpp"
//#include "flx_collector.hpp"
#include "flx_dynlink.hpp"
#include <stddef.h>

namespace flx { namespace rtl {
// ********************************************************
// OFFSETS for flx_dynlink_t
// ********************************************************
FLX_FINALISER(flx_dynlink_t)
::flx::gc::generic::gc_shape_t flx_dynlink_ptr_map = {
  NULL,
  "rtl::flx_dynlink_t",
  1,sizeof(flx_dynlink_t),
  flx_dynlink_t_finaliser, 
  0,
  0,
  ::flx::gc::generic::tblit<flx_dynlink_t>,::flx::gc::generic::tunblit<flx_dynlink_t>, 
  ::flx::gc::generic::gc_flags_default,
  0UL, 0UL
};


// ********************************************************
// OFFSETS for flx_libinst 
// ********************************************************
static const std::size_t flx_libinst_offsets[4]={
    offsetof(flx_libinst_t,thread_frame),
    offsetof(flx_libinst_t,start_proc),
    offsetof(flx_libinst_t,main_proc),
    offsetof(flx_libinst_t,lib)
};
FLX_FINALISER(flx_libinst_t)
static ::flx::gc::generic::offset_data_t const flx_libinst_offset_data = { 4, flx_libinst_offsets };
::flx::gc::generic::gc_shape_t flx_libinst_ptr_map = {
  &flx_dynlink_ptr_map,
  "rtl::flx_libinst",
  1,sizeof(flx_libinst_t),
  flx_libinst_t_finaliser, 
  &flx_libinst_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<flx_libinst_t>,::flx::gc::generic::tunblit<flx_libinst_t>, 
  ::flx::gc::generic::gc_flags_default,
  0UL, 0UL
};



// ********************************************************
//OFFSETS for slist_node_t
// ********************************************************
static const std::size_t slist_node_offsets[2]={
    offsetof(slist_node_t,next),
    offsetof(slist_node_t,data)
};

static ::flx::gc::generic::offset_data_t const slist_node_offset_data = { 2, slist_node_offsets };
::flx::gc::generic::gc_shape_t slist_node_ptr_map = {
  &flx_libinst_ptr_map,
  "rtl::slist_node_t",
  1,sizeof(slist_node_t),
  0, // no finaliser,
  &slist_node_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<slist_node_t>,::flx::gc::generic::tunblit<slist_node_t>, 
  ::flx::gc::generic::gc_flags_default,
  0UL, 0UL
};


// ********************************************************
//OFFSETS for slist_t
// ********************************************************
static const std::size_t slist_offsets[1]={
    offsetof(slist_t,head)
};
static ::flx::gc::generic::offset_data_t const slist_offset_data = { 1, slist_offsets };

::flx::gc::generic::gc_shape_t slist_ptr_map = {
  &slist_node_ptr_map,
  "rtl::slist_t",
  1,sizeof(slist_t),
  0, // no finaliser
  &slist_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<slist_t>,::flx::gc::generic::tunblit<slist_t>, 
  ::flx::gc::generic::gc_flags_default,
  0UL, 0UL
};


// ********************************************************
//OFFSETS for fthread_t
// ********************************************************
static const std::size_t _fthread_offsets[1]={
    offsetof(fthread_t,cc)
};

static ::flx::gc::generic::offset_data_t const _fthread_offset_data = { 1, _fthread_offsets };

::flx::gc::generic::gc_shape_t _fthread_ptr_map = {
  &slist_ptr_map,
  "rtl::fthread_t",
  1,sizeof(fthread_t),
  0,
  &_fthread_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<fthread_t>,::flx::gc::generic::tunblit<fthread_t>, 
  gc::generic::gc_flags_immobile,
  0UL, 0UL
};


// ********************************************************
//OFFSETS for schannel_t
// ********************************************************
static const std::size_t schannel_offsets[2]={
    offsetof(schannel_t,waiting_to_read),
    offsetof(schannel_t,waiting_to_write)
};

static ::flx::gc::generic::offset_data_t const schannel_offset_data = { 2, schannel_offsets };

::flx::gc::generic::gc_shape_t schannel_ptr_map = {
  &_fthread_ptr_map,
  "rtl::schannel_t",
  1,sizeof(schannel_t),
  0, // no finaliser
  &schannel_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<schannel_t>,::flx::gc::generic::tunblit<schannel_t>, 
  gc::generic::gc_flags_default,
  0UL, 0UL
};

// ********************************************************
// _uctor_ implementation
// ********************************************************
//OFFSETS for _uctor_
static const std::size_t _uctor_offsets[1]= {
  offsetof(_uctor_,data)
};

static ::flx::gc::generic::offset_data_t const _uctor_offset_data = { 1, _uctor_offsets };

::flx::gc::generic::gc_shape_t _uctor_ptr_map = {
  &schannel_ptr_map,
  "rtl::_uctor_",
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


::flx::gc::generic::gc_shape_t _int_ptr_map = {
  &_uctor_ptr_map,
  "rtl::int",
  1,
  sizeof(int),
  0,
  0,
  0,
  ::flx::gc::generic::tblit<int>,::flx::gc::generic::tunblit<int>, 
  gc::generic::gc_flags_default,
  0UL, 0UL
};

::flx::gc::generic::gc_shape_t cl_t_ptr_map = {
  &_int_ptr_map,
  "rtl::cl_t",
  1,
  sizeof(cl_t),
  0,
  0,
  0,
  ::flx::gc::generic::tblit<cl_t>,::flx::gc::generic::tunblit<cl_t>, 
  gc::generic::gc_flags_default,
  0UL, 0UL
};


// ********************************************************
// pointer implementation
// ********************************************************

//OFFSETS for address
static const std::size_t _address_offsets[1]={ 0 };
::flx::gc::generic::offset_data_t const _address_offset_data = { 1, _address_offsets };

static ::std::string address_encoder (void *p) { 
  return ::flx::gc::generic::blit (p,sizeof (void*));
}

static size_t address_decoder (void *p, char *s, size_t i) { 
  return ::flx::gc::generic::unblit (p,sizeof (void*),s,i);
}


// ********************************************************
// address implementation : MUST BE LAST because the compiler
// uses "address_ptr_map" as the back link for generated shape tables
// ********************************************************

::flx::gc::generic::gc_shape_t _address_ptr_map = {
  &cl_t_ptr_map,
  "rtl::address",
  1,
  sizeof(void*),
  0,
  &_address_offset_data,
  ::flx::gc::generic::scan_by_offsets,
  ::flx::gc::generic::tblit<void*>,::flx::gc::generic::tunblit<void*>, 
  gc::generic::gc_flags_default,
  0UL, 0UL
};


}}


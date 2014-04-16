#include "flx_world_config.hpp"
#include <cstdlib>

static double egetv(char const *name, double dflt)
{
  char *env = ::std::getenv(name);
  double val = env?::std::atof(env):dflt;
  return val;
}

namespace flx { namespace run {

// =================================================================
// // Constructor
// =================================================================
flx_config::flx_config 
(
  link_library_t link_library_arg,
  init_ptr_create_async_hooker_t init_ptr_create_async_hooker_arg,
  get_flx_args_config_t get_flx_args_config_arg
) :
  link_library(link_library_arg),
  init_ptr_create_async_hooker(init_ptr_create_async_hooker_arg),
  get_flx_args_config(get_flx_args_config_arg)
{}

// =================================================================
// Initialiser
// =================================================================

int
flx_config::init(int argc, char **argv) {
  if(get_flx_args_config(argc, argv, this)) return 1;

  debug = (bool)egetv("FLX_DEBUG", debug);
  if (debug) {
    fprintf(stderr,
      "[FLX_DEBUG] Debug enabled for %s link program\n",
      static_link ? "static" : "dynamic");
  }

  debug_threads = (bool)egetv("FLX_DEBUG_THREADS", debug);
  if (debug_threads) {
    fprintf(stderr, "[FLX_DEBUG_THREADS] Threads debug enabled\n");
  }

  debug_allocations = (bool)egetv("FLX_DEBUG_ALLOCATIONS", debug);
  if (debug_allocations) {
    fprintf(stderr, "[FLX_DEBUG_ALLOCATIONS] Allocation debug enabled\n");
  }

  debug_collections = (bool)egetv("FLX_DEBUG_COLLECTIONS", debug);
  if (debug_collections)
  {
    fprintf(stderr, "[FLX_DEBUG_COLLECTIONS] Collection debug enabled\n");
  }

  report_collections = (bool)egetv("FLX_REPORT_COLLECTIONS", debug);
  if (report_collections)
  {
    fprintf(stderr, "[FLX_REPORT_COLLECTIONS] Collection report enabled\n");
  }


  debug_driver = (bool)egetv("FLX_DEBUG_DRIVER", debug);
  if (debug_driver)
  {
    fprintf(stderr, "[FLX_DEBUG_DRIVER] Driver debug enabled\n");
  }

  finalise = (bool)egetv("FLX_FINALISE", 0);
  if (debug)
    fprintf(stderr,
      "[FLX_FINALISE] Finalisation %s\n", finalise ? "Enabled" : "Disabled");

  // default collection frequency is 1000 interations
  gc_freq = (unsigned long)egetv("FLX_GC_FREQ", 1000);
  if (gc_freq < 1) gc_freq = 1;
  if (debug)
    fprintf(stderr, "[FLX_GC_FREQ] call gc every %lu iterations\n", gc_freq);

  // default min mem is 10 Meg
  min_mem = (unsigned long)(egetv("FLX_MIN_MEM", 10) * 1000000.0);
  if (debug)
    fprintf(stderr, "[FLX_MIN_MEM] call gc only if more than %lu Meg heap used\n", min_mem/1000000);

  // default max mem is unlimited
  max_mem = (unsigned long)(egetv("FLX_MAX_MEM", 0) * 1000000.0);
  if (max_mem == 0) max_mem = (unsigned long)-1;
  if (debug)
    fprintf(stderr, "[FLX_MAX_MEM] terminate if more than %lu Meg heap used\n", max_mem/1000000);

  // default free factor is 10%, this is also the minimum allowed
  free_factor = egetv("FLX_FREE_FACTOR", 1.1);
  if (free_factor < 1.1) free_factor = 1.1;
  if (debug)
    fprintf(stderr, "[FLX_FREE_FACTOR] reset gc trigger %4.2f times heap used after collection\n", free_factor);

  // experimental flag to allow collection anywhere
  // later, we default this one to true if we can
  // find all the thread stacks, which should be possible
  // with gcc and probably msvc++

  allow_collection_anywhere = (bool)egetv("FLX_ALLOW_COLLECTION_ANYWHERE", 1);
  if (debug)
    fprintf(stderr, "[FLX_ALLOW_COLLECTION_ANYWHERE] %s\n", allow_collection_anywhere ? "True" : "False");

  if (debug) {
    for (int i=0; i<flx_argc; ++i)
      fprintf(stderr, "flx_argv[%d]->%s\n", i, flx_argv[i]);
  }
  return 0;
}

}} // namespaces


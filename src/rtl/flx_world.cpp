#include "flx_world.hpp"
#include "flx_eh.hpp"
#include "flx_ts_collector.hpp"

using namespace std;
using namespace flx::rtl;
using namespace flx::pthread;
using namespace flx::run;

namespace flx { namespace rtl {

int do_final_cleanup(
  bool debug_driver,
  flx::gc::generic::gc_profile_t *gcp,
  flx_dynlink_t *library,
  flx_libinit_t *instance
);

static void doflx_pthread_entry(void *data) {
  doflx_data *d = (doflx_data*)data;
  while(d->doflx()) ;
  delete d;
}

bool doflx_data::doflx() {
  try
  {
    if (debug_driver)
      fprintf(stderr, "doflx: Process active ..");

    if (debug_driver)
      fprintf(stderr, "dofx: Before running: Sync state is %s/%s\n",
        get_fstate_desc(ss.fs), get_fpc_desc(ss.pc));

    ss.frun();

    if (debug_driver)
      fprintf(stderr, "doflx: After running: Sync state is %s/%s\n",
        get_fstate_desc(ss.fs), get_fpc_desc(ss.pc));

    //fprintf(stderr, "Thread yielding ..");
    //thread_control->yield();
    //fprintf(stderr, "..Thread resuming!\n");

    if (FLX_LIKELY(ss.fs == delegated)) {
      switch (ss.request->variant) {
        case svc_collect:
        {
          gcp->actually_collect();
        }
          return true;

        case svc_spawn_pthread:
        {
          fthread_t *ftx = *(fthread_t**)ss.request->data;
          if (debug_driver)
            fprintf(stderr, "doflx: Spawn pthread %p\n", ftx);
          gcp->collector->add_root(ftx);
          std::list<fthread_t*> *pactive = new std::list<fthread_t*>;
          pactive->push_front(ftx);
          void *data = new doflx_data(world,debug_driver, gcp, pactive, thread_control);
          flx_detached_thread_t dummy;

          if (debug_driver)
            fprintf(stderr, "doflx: Starting new pthread, thread counter= %d\n",
              thread_control->thread_count());

          {
            flx_mutex_t spawner_lock;
            flx_condv_t spawner_cond;
            bool spawner_flag = false;
            flx_mutex_locker_t locktite(spawner_lock);
            dummy.init(doflx_pthread_entry, data, thread_control,
              &spawner_lock, &spawner_cond,
              &spawner_flag
            );

            if (debug_driver)
              fprintf(stderr,
                "Thread %p waiting for spawned thread to register itself\n",
                (void*)get_current_native_thread());

            while (!spawner_flag)
              spawner_cond.wait(&spawner_lock);

            if (debug_driver)
              fprintf(stderr,
                "Thread %p notes spawned thread has registered itself\n",
                (void*)get_current_native_thread());
          }
        }
          return true;

        case svc_general:
        {
          if (debug_driver)
            fprintf(stderr, "doflx: svc_general from fthread=%p\n", ss.ft);

          if(debug_driver)
            fprintf(stderr, "doflx: async=%p, ptr_create_async_hooker=%p\n", async,world->c->ptr_create_async_hooker);
          if (!async) {
            if(debug_driver)
              fprintf(stderr,"doflx: trying to create async system..\n");

            if (world->c->ptr_create_async_hooker == NULL) {
              if(debug_driver)
                fprintf(stderr,"doflx: trying to create async hooker..\n");
              world->c->init_ptr_create_async_hooker(world->c,debug_driver);
            }
            // Error out if we don't have the hooker function.
            if (world->c->ptr_create_async_hooker == NULL) {
              fprintf(stderr,
                "doflx: Unable to initialise async I/O system: terminating\n");
              exit(1);
            }

            async = (*world->c->ptr_create_async_hooker)(
              20000, // bound on resumable thread queue
              50,    // bound on general input job queue
              2,     // number of threads in job pool
              50,    // bound on async fileio job queue
              1      // number of threads doing async fileio
            );
          }
          ++async_count;
          // CHANGED TO USE NEW UNION LAYOUT RULES
          // One less level of indirection for pointers
          // void *dreq =  *(void**)ss.request->data;
          void *dreq =  (void*)ss.request->data;

          // requests are now ALWAYS considered asynchronous
          // even if the request handler reschedules them immediately
          async->handle_request(dreq, ss.ft);
          ss.pc = next_fthread_pos;
        }
          return true;

        default:
          fprintf(stderr,
            "doflx: Unknown service request code 0x%4x\n", ss.request->variant);
          abort();
      }
    }

    switch(ss.fs) {
      case blocked: // ran out of active threads - are there any in the async queue?
        if(do_async())
          return true;
        break;
      case terminated:
        break;
      default:
        fprintf(stderr, "doflx: Unknown frun return status 0x%4x\n", ss.fs);
        abort();
    }
  }
  catch (flx_exception_t &x) { flx_exception_handler (&x); }
  catch (std::exception &x) { std_exception_handler (&x); }
  catch (int &x) { fprintf (stderr, "Exception type int: %d\n", x); }
  catch (::std::string &x) { fprintf (stderr, "Exception type string : %s\n", x.c_str()); }
  catch (...) { fprintf(stderr, "Unknown exception in thread!\n"); }


  if (debug_driver)
    fprintf(stderr, "doflx: Out of jobs\n");
  return false;
}

bool doflx_data::do_async() {
  if (debug_driver) {
    fprintf(stderr,
      "doflx: out of active synchronous threads, trying async, count=%ld\n", async_count);
  }

  if (async && async_count > 0) {
    // STILL A ROOT
    fthread_t* ftp = async->dequeue();

    if (debug_driver)
      fprintf(stderr, "doflx: Async Retrieving fthread %p\n", ftp);

    active->push_front(ftp);
    --async_count;
    ss.pc = next_fthread_pos;
    return true;
  }
  return false;
}

doflx_data::~doflx_data() {
  try
  {
    if (debug_driver)
      fprintf(stderr, "doflx: Terminating Felix subsystem\n");
    delete async;
    delete active;
  }
  catch (...) { fprintf(stderr, "Unknown exception deleting async!\n"); }
}

// RUN A FELIX INSTANCE IN THE CURRENT PTHREAD
//
// CURRENTLY ONLY CALLED ONCE IN MAIN THREAD

static void *get_stack_pointer() { void *x=(void*)&x; return x; }

std::list<fthread_t*>*run_felix_pthread_ctor(
  flx::gc::generic::gc_profile_t *gcp,
  flx_libinit_t *instance)
{
  //fprintf(stderr, "run_felix_pthread -- the MAIN THREAD\n");
  flx::gc::generic::collector_t *collector = gcp->collector;
  std::list<fthread_t*> *active = new std::list<fthread_t*>;

  {
    con_t *top = instance->main_proc;
    if (top)
    {
      fthread_t *flx_main = new (*gcp, _fthread_ptr_map, false) fthread_t(top);
      collector->add_root(flx_main);
      active->push_front(flx_main);
    }
  }

  {
    con_t *top = instance->start_proc;
    fthread_t *ft = new (*gcp, _fthread_ptr_map, false) fthread_t(top);
    collector->add_root(ft);
    active->push_front(ft);
  }
  return active;
}

void run_felix_pthread_dtor(
  bool debug_driver,
  flx::gc::generic::gc_profile_t *gcp,
  flx::pthread::thread_control_t *thread_control,
  flx_dynlink_t *library,
  flx_libinit_t *instance
)
{
  if (debug_driver)
    fprintf(stderr, "MAIN THREAD FINISHED: waiting for other threads\n");

  thread_control->join_all();

  if (debug_driver) fprintf(stderr, "ALL THREADS DEAD: mainline cleanup!\n");

  if (debug_driver) {
    flx::gc::generic::collector_t *collector = gcp->collector;

    unsigned long uncollected = collector->get_allocation_count();
    unsigned long roots = collector->get_root_count();
    fprintf(stderr,
      "program finished, %ld collections, %ld uncollected objects, roots %ld\n",
      gcp->collections, uncollected, roots);
  }
  if (gcp->finalise)
    (void)do_final_cleanup(debug_driver, gcp, library, instance);
}

// terminates process!
// Not called by default (let the OS clean up)
//
// NEEDS TO BE SPLIT UP so that destroying
// a program instance is separated from unloading
// the library

int do_final_cleanup(
  bool debug_driver,
  flx::gc::generic::gc_profile_t *gcp,
  flx_dynlink_t *library,
  flx_libinit_t *instance
)
{
  flx::gc::generic::collector_t *collector = gcp->collector;

  // garbage collect application objects
  {
    if (debug_driver || gcp->debug_collections)
      fprintf(stderr, "Finalisation: pass 1 Data collection starts ..\n");

    unsigned long n = collector->collect();
    unsigned long a = collector->get_allocation_count();

    if (debug_driver || gcp->debug_collections)
      fprintf(stderr, "flx_run collected %ld objects, %ld left\n", n, a);
  }

  // Destroy program instance/ thread frame object

  if (debug_driver) fprintf(stderr, "Destroying program instance\n");
  instance->destroy();

  // garbage collect system objects
  {
    if (debug_driver || gcp->debug_collections)
      fprintf(stderr, "Finalisation: pass 2 Final collection starts ..\n");

    collector->free_all_mem();
    unsigned long a = collector->get_allocation_count();

    if (debug_driver || gcp->debug_collections)
      fprintf(stderr, "Remaining %ld objects (should be 0)\n", a);

    if (a != 0){
      fprintf(stderr, "flx_run %ld uncollected objects, should be zero!!\n", a);
      return 5;
    }
  }

  // dump the DLL

  if (debug_driver)
    fprintf(stderr, "Libref cnt = %ld\n", library->refcnt);

  if (library->refcnt > 0)
  {
    if (debug_driver)
      fprintf(stderr,
        "flx_run %p library still referenced %ld times\n",
        library->library, library->refcnt);
  }

  if (debug_driver)
    fprintf(stderr, "Unlinking library ..\n");

  library->unlink();
  return 0;
}

static double egetv(char const *name, double dflt)
{
  char *env = std::getenv(name);
  double val = env?atof(env):dflt;
  return val;
}

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
  min_mem = (unsigned long)egetv("FLX_MIN_MEM", 10) * 1000000.0;
  if (debug)
    fprintf(stderr, "[FLX_MIN_MEM] call gc only if more than %lu Meg heap used\n", min_mem/1000000);

  // default max mem is unlimited
  max_mem = (unsigned long)egetv("FLX_MAX_MEM", -1) * 1000000.0;
  if (max_mem <= 0) max_mem = (unsigned long)-1;
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

// construct from flx_config pointer
flx_world::flx_world(flx_config *c_arg) : c(c_arg) {}

int flx_world::setup(int argc, char **argv) {
  int res;
  if((res = c->init(argc, argv) != 0)) return res;

  debug = c->debug;
  debug_driver = c->debug_driver;

  allocator = new flx::gc::collector::malloc_free();
  allocator->set_debug(c->debug_allocations);

  // previous direct ctor scope ended at closing brace of FLX_MAIN
  // but delete can probably be moved up after collector delete (also used by explicit_dtor)
  thread_control = new flx::pthread::thread_control_t(c->debug_threads);

  // NB: !FLX_SUPPORT_ASYNC refers to async IO, hence ts still needed thanks to flx pthreads
  collector = new flx::gc::collector::flx_ts_collector_t(allocator, thread_control);
  collector->set_debug(c->debug_collections);

  gcp = new flx::gc::generic::gc_profile_t(
    c->debug_allocations,
    c->debug_collections,
    c->report_collections,
    c->allow_collection_anywhere,
    c->gc_freq,
    c->min_mem,
    c->max_mem,
    c->free_factor,
    c->finalise,
    collector
  );

  library = c->link_library(c);

  if (debug_driver)
    fprintf(stderr, "flx_run driver begins %s\n", c->flx_argv[0]);

  // flx_libinit_t::create can run code, so add thread to avoid world_stop abort
  thread_control->add_thread(get_stack_pointer());

  // Create the usercode driver instance
  // NB: seems to destroy()ed in do_final_cleanup
  instance.create(
    library,
    gcp,
    library->main_sym,
    c->flx_argc,
    c->flx_argv,
    stdin,
    stdout,
    stderr);

  thread_control->remove_thread();

  if (debug_driver) {
    fprintf(stderr, "loaded library %s at %p\n", c->filename, library->library);
    fprintf(stderr, "thread frame at %p\n", instance.thread_frame);
    fprintf(stderr, "initial continuation at %p\n", instance.start_proc);
    fprintf(stderr, "main continuation at %p\n", instance.main_proc);
  }

  dfd = new doflx_data(
    this,
    debug_driver,
    gcp,
    run_felix_pthread_ctor(gcp, &instance),
    thread_control); // deletes active for us!

  return 0;
}

int flx_world::explicit_dtor()
{
  run_felix_pthread_dtor(debug_driver, gcp, thread_control, library, &instance);

  if (gcp->finalise)
  {
    if (library->refcnt > 0)
    {
      fprintf(stderr,
        "flx_run %p library still referenced %ld times?!\n",
        library->library, library->refcnt);
      return 6;
    }
    if (debug_driver)
      fprintf(stderr, "flx_run driver ends with finalisation complete\n");
  }
  else
  {
    if (debug_driver || gcp->debug_collections)
    {
      unsigned long a = gcp->collector->get_allocation_count();
      fprintf(stderr,
        "flx_run driver ends with finalisation skipped, %ld uncollected "
          "objects\n", a);
    }
  }

  return 0;
}

int flx_world::teardown() {
  delete dfd;

  // could this override error_exit_code if something throws?
  int error_exit_code = explicit_dtor();
  delete library;

  // And we're done, so start cleaning up.
  delete gcp;
  delete collector;
  delete allocator;

  if (debug) fprintf(stderr, "flx_run driver ends code=%d\n", error_exit_code);

  delete thread_control;  // RF: cautiously delete here
  return error_exit_code;
}

bool flx_world::run_until_blocked() {
  // this may not be called on the same thread, so let thread control know
  // when we exit, main thread is not running so pthreads can garbage collect without waiting for us
  thread_control->add_thread(get_stack_pointer());

  bool    res = dfd->doflx();

  thread_control->remove_thread();

  return res;
}

}} // namespaces

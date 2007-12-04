#include <time.h>
//#include <unistd.h>

#include <cstdio>
#include <cstring>
#include <cassert>

#include <vector>
#include <list>
#include <map>
#include <string>
#include <iostream>
#include <sstream>

#include "flx_rtl.hpp"
#include "flx_collector.hpp"
#include "flx_dynlink.hpp"

using namespace flx::rtl;
using namespace flx::gc::generic;
using namespace flx::gc::collector;
using namespace std;

#ifdef FLX_STATIC_LINK
extern void *line_handler_creator;
#endif


typedef string message_t;
int niter = 1000;
int xcounter = 0;
int xcounter2 = 0;
bool run = true;
message_t read_message()
{
  ++ xcounter;
  if (xcounter > 1000) {
    xcounter = 1;
    xcounter2++;
    if(xcounter2>=niter) run = false;
  }
  stringstream ss;
  ss << (xcounter & ~1) << " " << xcounter << " TEXT";
  return ss.str();
}


int id_of_message(string const &msg)
{
//  return msg >> 1;

  int id = 0;
  char const*p = msg.data();
  for(; *p; ++p)
    if(isdigit(*p)) break;

  for(; *p; ++p)
  {
    if(!isdigit(*p)) break;
    else id = id * 10 + int(*p)-int('0');
  }
  //printf("Message id is %d\n",id);
  return id;
}

struct library_linkage_t : flx_dynlink_t
{
  typedef con_t *(*line_handler_creator_t)
  (
    thread_frame_t*
  );

  // interface
  line_handler_creator_t create_line_handler;

  // find dlsyms from library
  void usr_link()
  {
    create_line_handler = (line_handler_creator_t)
      DLSYM(library,line_handler_creator);
    if(!create_line_handler) {
      printf("Unable to get line handler creator procedure from library\n");
      exit(1);
    }
  }
};


int main(int argc, char** argv)
{

  if (argc<2)
  {
    printf("usage flx_perf_drv ./<shared library name>\n");
    exit(1);
  }
  if(argc==3)
  {
    // each loop is 1K transactions
    char* endptr = NULL;
    niter = strtol(argv[2], &endptr, 0);
    if (!(endptr && *endptr == '\0')) {
      printf("invalid number of iterations: '%s'\n", argv[2]);
      exit(1);
    }
  }
  try
  {
    printf("Running flx_perf_drv, %dK transactions\n", niter );
    //sleep(1);
    library_linkage_t library;
    flx_libinit_t instance;

    library.link(argv[1]);
    malloc_free allocator;
    flx_collector_t collector(&allocator, NULL);
    gc_profile_t *gcp = new flx::gc::generic::gc_profile_t(
      false,
      false,
      false,
      0,
      -1,
      -1,
      0,
      true,
      &collector
    );
    instance.create(&library, gcp,library.main_sym,0,NULL,stdin,stdout,stderr);

    typedef int session_key;
    typedef pair<const session_key, fthread_t*> session_record;

    std::list<session_record> active;
    std::map<session_key,fthread_t*> waiting;

    fthread_t *ft// =
      //new(collector,flx::rtl::_fthread_ptr_map)
      //fthread_t(instance.start_proc)
    ;

    active.push_front(make_pair(-1,ft));
    collector.add_root(ft);
    int gc_counter = 0;

    time_t start = time(NULL);
    while (run)
    {
      // process all yielded threads until they block or die
      while(active.size() > 0)
      {
        assert(active.size()<100);
        session_record top_rec = *(active.begin());
        int active_thread_id FLX_UNUSED = top_rec.first;
        //printf("Processing active thread %d\n",top_rec.first);
        fthread_t *top = top_rec.second;
        active.pop_front();
        assert(top);
        _uctor_ *request = top->run();
        if(request)
        {
          assert (request->variant == svc_read);
          try {
            waiting.insert(make_pair(top_rec.first, top));
            assert(waiting.size()<100);
          }
          catch (...) {
            fprintf(stderr,"INSERT FAILED\n");
            exit(0);
          }
        }
        else {
          //printf("THREAD SUICIDES %p\n",ft);
          collector.remove_root(top);
        }
      }

      //printf("Out of active threads\n");
      // read a message, calculate it's session id
      message_t msg = read_message();
      //printf("Message =%s\n",msg.data());
      int id = id_of_message(msg);
      //printf("New message Id=%d\n",id);

      // find the id in the map of waiting
      std::map<int,fthread_t*>::iterator line_handler_ptr =
        waiting.find(id);

      if(line_handler_ptr  != waiting.end())
      {
        //printf("Found existing WAITING line handler for %d, making active\n", id);
        active.push_front(*line_handler_ptr);

        **(message_t**)((*line_handler_ptr).second->get_svc()->data) = msg;
        waiting.erase(line_handler_ptr);
      }
      else
      {
        //printf("creating new instance %d\n", id);
        gc_counter++;
        if(gc_counter==100) {
          //printf("TRIGGERING GC CYCLE\n");
          //printf("Total memory allocated=%ld\n",collector.get_allocation_amt());
          int collected FLX_UNUSED = collector.collect();
          //printf("Collected %d unreachable objects\n", collected);
          //printf("Total memory allocated=%ld\n",collector.get_allocation_amt());
          if(collector.get_allocation_count()>100)
            printf("WARNING!: Currently allocated=%ld\n",
            collector.get_allocation_count());
          gc_counter = 0;
        }
        con_t *top =
          library.create_line_handler(
            instance.thread_frame
          )
        ;
        fthread_t *ft;// = new(collector,flx::rtl::_fthread_ptr_map) fthread_t(top);
        collector.add_root(ft);
        //printf("instance created, continuation=%p\n",top);
        _uctor_ *request = ft->run();
        if(request)
        {
          assert (request->variant=svc_read);
          //printf("instance waiting for INITIAL message\n");
          try {
            active.push_front (make_pair(id,ft));
            assert(active.size()<100);
          }
          catch (...) {
            fprintf(stderr,"PUSH FRONT3 failed\n");
            exit(1);
          }

          if (request->data)
          {
            **(message_t**)(request->data) = msg;
          }
          else
          {
            printf("SERVICE ERROR: waiting for message without setting address\n");
            exit(1);
          }
        }
        else
          printf("Thread died before reading INITIAL message\n") ;
      }
    }
    time_t finished = time(NULL);
    time_t seconds = finished - start;
    int trxrate = niter / seconds;

    printf("All services terminated .. %d K trx/sec real-time\n",trxrate);
    printf("%ld garbage objects, %ld bytes\n",
      collector.get_allocation_count(),
      collector.get_allocation_amt()
    );
    collector.collect();
    instance.destroy();
    printf("flx_perf_drv ends normally\n");
    exit(0);
  }
  catch (...) {
    fprintf(stderr,"EXCEPTION CAUGHT\n");
  }
}

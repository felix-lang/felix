#include "flx_dynlink.hpp"
#include "flx_strutil.hpp"
#include <stdio.h>
#include <cstring>
#include <cstdlib>

namespace flx { namespace rtl {

#if FLX_MACOSX_NODLCOMPAT
void*
getmachosym(NSModule library, const char* symname)
{
    NSSymbol    sym = NSLookupSymbolInModule(library, symname);
    if(sym)
        return NSAddressOfSymbol(sym);
    return 0;
}

#endif

LIBHANDLE
flx_load_library(const std::string& filename)
{
  LIBHANDLE library;
  FLX_SET_NOLIBRARY(library);
  if (std::getenv("FLX_SHELL_ECHO")!=(char*)0)
    fprintf(stderr,"[load_library] %s\n", filename.c_str());
//#if FLX_WIN32 || FLX_CYGWIN
#if FLX_WIN32
  // stop windows showing err dialogues, ignoring error code.
  (void)SetErrorMode(SEM_NOOPENFILEERRORBOX);
  library = LoadLibrary(filename.c_str());
  if(FLX_CHECK_NOLIBRARY(library))
    throw flx_link_failure_t(filename,"LoadLibrary","Cannot find dll");
#else
  #if FLX_MACOSX_NODLCOMPAT
    NSObjectFileImage            bndl_img;
    NSObjectFileImageReturnCode  res;

    string s_filename(filename);
    struct stat st;

    if (stat(s_filename.c_str(), &st) != 0) {
      // we can't find the file, so search DYLD_LIBRARY_PATH
      string paths = getenv("DYLD_LIBRARY_PATH");

      size_t i = 0;
      bool found = false;

      while ((i = paths.find_first_of(':')) != paths.npos) {
        string s = paths.substr(0, i) + '/' + filename;
        paths = paths.substr(i + 1);

        if (stat(s.c_str(), &st) == 0) {
          s_filename = s;
          found = true;
          break;
        }
      }

      if (!found) {
              throw flx_link_failure_t(filename, "stat",
                string("cannot find file: ") + filename);
      }
    }

    res = NSCreateObjectFileImageFromFile(s_filename.c_str(), &bndl_img);

    if(NSObjectFileImageSuccess != res)
      throw flx_link_failure_t(filename, "NSCreateObjectFileImageFromFile",
              "failure to open library");

    // don't merge globals with loader's, load programmatically
    // return on error allows us to continue without being terminated

    unsigned long link_flags;
    link_flags = NSLINKMODULE_OPTION_PRIVATE | NSLINKMODULE_OPTION_RETURN_ON_ERROR;
    library = NSLinkModule(bndl_img, filename.c_str(), link_flags);

    // even if link failed, we do this
    NSDestroyObjectFileImage(bndl_img);

    // more info can be gleaned about link errors from NSLinkEditError
    if(FLX_CHECK_NOLIBRARY(library))
      throw flx_link_failure_t(filename, "NSLinkModule", "failed to link");

  #else
    library = dlopen(filename.c_str(),RTLD_NOW);
    if(FLX_CHECK_NOLIBRARY(library))
      throw flx_link_failure_t(filename,"dlopen",dlerror());
  #endif
#endif
  return library;
}

LIBHANDLE
flx_load_module(const std::string& filename)
{
  return flx_load_library(filename + FLX_LIB_EXTENSION);
}

flx_dynlink_t::~flx_dynlink_t() {}
flx_dynlink_t::flx_dynlink_t(flx_dynlink_t const&) {} // no copy hack
void flx_dynlink_t::operator=(flx_dynlink_t const&) {} // no copy hack

flx_dynlink_t::flx_dynlink_t():
  filename(""),
  modulename(""),
  library(0),
  thread_frame_creator(NULL),
  start_sym(NULL),
  main_sym(NULL),
  refcnt(0)
{}

flx_dynlink_t::flx_dynlink_t(
  ::std::string modulename_a,
  thread_frame_creator_t thread_frame_creator,
  start_t start_sym,
  main_t main_sym) throw(flx_link_failure_t)
:
  modulename (modulename_a),
  library(0),
  thread_frame_creator(thread_frame_creator),
  start_sym(start_sym),
  main_sym(main_sym),
  refcnt(0)
{
  if(!thread_frame_creator)
    throw flx_link_failure_t("<static link>","dlsym","create_thread_frame");

  if(!start_sym)
    throw flx_link_failure_t("<static link>","dlsym","flx_start");
}

void flx_dynlink_t::link (
  ::std::string modulename,
  thread_frame_creator_t thread_frame_creator,
  start_t start_sym,
  main_t main_sym
)
{
  this->modulename = modulename;
  this->thread_frame_creator = thread_frame_creator;
  this->start_sym = start_sym;
  this->main_sym = main_sym;
}


void flx_dynlink_t::link(const ::std::string& filename_a, const ::std::string& modulename_a) throw(flx_link_failure_t)
{
  filename = filename_a;
  modulename = modulename_a;
  library = flx_load_library(filename);
  //fprintf(stderr,"File %s dlopened at %p ok\n",fname.c_str(),library);

  thread_frame_creator = (thread_frame_creator_t)
    FLX_SDLSYM(library,(modulename+"_create_thread_frame").c_str());
  if(!thread_frame_creator)
    throw flx_link_failure_t(filename,"dlsym",modulename+"_create_thread_frame");

  //fprintf(stderr,"Thread frame creator found at %p\n",thread_frame_creator);

  start_sym = (start_t)FLX_SDLSYM(library,(modulename+"_flx_start").c_str());
  if(!start_sym)
    throw flx_link_failure_t(filename,"dlsym",modulename+"_flx_start");

  main_sym = (main_t)FLX_DLSYM(library,flx_main);

  //fprintf(stderr,"Start symbol found at %p\n",start_sym);
  //fprintf(stderr,"main symbol found at %p\n",main_sym);

  refcnt = 1L;

  //fprintf(stderr,"Set refcnt to 1\n");
  try { usr_link(); }
  catch (flx_link_failure_t &) { throw; }
  catch (...) {
    throw flx_link_failure_t
    (
      filename,
      "usr_link()",
      "Unknown user exception"
    );
  }
}

void flx_dynlink_t::link(const ::std::string& filename_a) throw(flx_link_failure_t)
{
  string mname = ::flx::rtl::strutil::filename_to_modulename (filename_a);
  link(filename_a,mname);
}

void flx_dynlink_t::unlink()
{
  --refcnt;
  if(refcnt == 0) {
    //fprintf(stderr,"closing library\n");
//#if FLX_WIN32 || FLX_CYGWIN
#if FLX_WIN32
    //FreeLibrary(library);
#else
  #if FLX_MACOSX_NODLCOMPAT
    //NSUnLinkModule(library, NSUNLINKMODULE_OPTION_NONE);
  #else
    //dlclose(library);
  #endif
#endif
  }
}

void flx_dynlink_t::usr_link(){}

flx_libinit_t::~flx_libinit_t(){}
flx_libinit_t::flx_libinit_t() :
  thread_frame (NULL),
  start_proc (NULL),
  main_proc (NULL),
  lib (NULL),
  gcp(NULL)
{}

flx_libinit_t::flx_libinit_t(flx_libinit_t const&){}
void flx_libinit_t::operator=(flx_libinit_t const&){}

void flx_libinit_t::create
(
  flx_dynlink_t *lib_a,
  flx::gc::generic::gc_profile_t *gcp_a,
  main_t main_sym,
  int argc,
  char **argv,
  FILE *stdin_,
  FILE *stdout_,
  FILE *stderr_
)
{
  lib = lib_a;
  gcp = gcp_a;
  //fprintf(stderr, "Creating thread frame\n");
  thread_frame = lib->thread_frame_creator( gcp);
  //fprintf(stderr, "thread frame CREATED\n");
  //fprintf(stderr,"Incrementing refcnt\n");
  ++lib->refcnt;
  gcp->collector->add_root(thread_frame);
  //fprintf(stderr, "CREATING start_proc\n");
  start_proc = lib->start_sym(thread_frame, argc, argv, stdin_,stdout_,stderr_);
  //fprintf(stderr, "start_proc CREATED\n");
  //fprintf(stderr, "CREATING main_proc\n");
  main_proc = main_sym?main_sym(thread_frame):0;
  //fprintf(stderr, "main_proc CREATED\n");
  usr_create();
}

void flx_libinit_t::usr_create(){
  //fprintf(stderr,"libinit done\n");
}

void flx_libinit_t::destroy () {
  usr_destroy();
  gcp->collector->remove_root(thread_frame);
  //fprintf(stderr,"[flx_libinit::destroy()] Removed thread frame %p as root\n", thread_frame);
  //fprintf(stderr,"Decrementing refcnt\n");
  //fprintf(stderr,"Start        Ref cnt=%ld\n",lib->refcnt);
  --lib->refcnt;
  //fprintf(stderr,"After decr:  Ref cnt=%ld\n",lib->refcnt);
}

void flx_libinit_t::usr_destroy (){}

con_t *flx_libinit_t::bind_proc(void *fn, void *data) {
  typedef con_t *(*binder_t)(void *,void*);
  return ((binder_t)fn)(thread_frame,data);
}

}} // namespaces

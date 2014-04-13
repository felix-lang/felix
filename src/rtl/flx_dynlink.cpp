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

flx_dynlink_t::flx_dynlink_t(flx_dynlink_t const&) {} // no copy hack
void flx_dynlink_t::operator=(flx_dynlink_t const&) {} // no copy hack

flx_dynlink_t::flx_dynlink_t():
  filename(""),
  modulename(""),
  library(0),
  thread_frame_creator(NULL),
  start_sym(NULL),
  main_sym(NULL)
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
  main_sym(main_sym)
{
  if(!thread_frame_creator)
    throw flx_link_failure_t("<static link>","dlsym","create_thread_frame");

  if(!start_sym)
    throw flx_link_failure_t("<static link>","dlsym","flx_start");
}

void flx_dynlink_t::static_link (
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


void flx_dynlink_t::dynamic_link_with_modulename(const ::std::string& filename_a, const ::std::string& modulename_a) throw(flx_link_failure_t)
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

}

flx_link_failure_t *flx_dynlink_t::nothrow_dynamic_link_with_modulename(
  const ::std::string& filename_a, const ::std::string& modulename_a)
{
  try { dynamic_link_with_modulename (filename_a, modulename_a); return NULL; }
  catch (flx_link_failure_t const &x) { return new flx_link_failure_t(x); }
}

void flx_dynlink_t::dynamic_link(const ::std::string& filename_a) throw(flx_link_failure_t)
{
  string mname = ::flx::rtl::strutil::filename_to_modulename (filename_a);
  dynamic_link_with_modulename(filename_a,mname);
}

flx_link_failure_t *flx_dynlink_t::nothrow_dynamic_link(const ::std::string& filename_a)
{
  try { dynamic_link(filename_a); return NULL; }
  catch (flx_link_failure_t const &x) { return new flx_link_failure_t(x); }
}

// dont actually unload libraries
// it doesn't work right in C/C++
// can leave dangling references
// impossible to manage properly
void flx_dynlink_t::unlink()
{
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

flx_dynlink_t::~flx_dynlink_t() { }

// ************************************************
// libinst
// ************************************************

flx_libinst_t::~flx_libinst_t(){}
flx_libinst_t::flx_libinst_t() :
  thread_frame (NULL),
  start_proc (NULL),
  main_proc (NULL),
  lib (NULL),
  gcp(NULL),
  debug(false)
{}

flx_libinst_t::flx_libinst_t(flx_libinst_t const&){}
void flx_libinst_t::operator=(flx_libinst_t const&){}

void flx_libinst_t::create
(
  flx_dynlink_t *lib_a,
  flx::gc::generic::gc_profile_t *gcp_a,
  main_t main_sym,
  int argc,
  char **argv,
  FILE *stdin_,
  FILE *stdout_,
  FILE *stderr_,
  bool debug_
)
{
  lib = lib_a;
  gcp = gcp_a;
  debug = debug_;
  if (debug)
    fprintf(stderr, "[libinst:create] Creating thread frame\n");
  thread_frame = lib->thread_frame_creator( gcp);
  if (debug)
    fprintf(stderr, "[libinst:create] thread frame CREATED\n");
  if (debug)
    fprintf(stderr, "[libinst:create] CREATING start_proc by running start_sym %p\n", lib->start_sym);
  start_proc = lib->start_sym(thread_frame, argc, argv, stdin_,stdout_,stderr_);
  if (debug)
    fprintf(stderr, "[libinst:create] start_proc CREATED %p\n", start_proc);
  if (debug)
    fprintf(stderr, "[libinst:create] CREATING main_proc by running main_sym %p\n", main_sym);
  main_proc = main_sym?main_sym(thread_frame):0;
  if (debug)
    fprintf(stderr, "[libinst:create] main_proc CREATED %p\n", main_proc);
}

con_t *flx_libinst_t::bind_proc(void *fn, void *data) {
  typedef con_t *(*binder_t)(void *,void*);
  return ((binder_t)fn)(thread_frame,data);
}

}} // namespaces

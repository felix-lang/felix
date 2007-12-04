import os
import sys

import fbuild.flxbuild
from fbuild.flxbuild.flxutil import ExecutionError
from fbuild.flxbuild.c_cxx_base import c_cxx_base

class gnu_mixin(object):
  DEFAULT_AR = 'ar -rc'
  DEFAULT_RANLIB = 'ranlib'

  def set_options(self, STRIP="strip", **kwds):
    """these options are model dependent and have to
    be supplied by the client"""

    super(gnu_mixin, self).set_options(**kwds)

    opt = self.options
    opt.STRIP = STRIP

    self.lang = opt.COM

    if opt.model == "nocygwin":
      opt.COM = opt.COM + "-mno-cygwin"
      opt.EXT_STATIC_OBJ = "_static.obj" # temporary hack
      opt.EXT_SHARED_OBJ = "_dynamic.obj" # temporary hack

  ########

  def detect_model(self):
    opt = self.options

    # default options
    opt.EXT_STATIC_OBJ = "_static.o"
    opt.EXT_SHARED_OBJ = "_dynamic.o"
    opt.EXT_LIB = ".a"
    opt.EXT_EXE = ""
    opt.EXT_SHLIB = ".so"
    opt.EXT_DYLIB = ".so"
    opt.SHLIB_DIR = os.path.join("lib", "rtl")

    opt.SPEC_COMPILE_OBJ = "-c"
    opt.SPEC_OBJ_FILENAME = "-o "
    opt.SPEC_EXE_FILENAME = "-o "
    opt.SPEC_DEFINE = "-D"
    opt.SPEC_INCLUDE = "-I"
    opt.SPEC_LIBPATH = "-L"
    opt.SPEC_LIB = "-l"
    opt.SPEC_AR_OUT_FILENAME = ""
    opt.DEBUG_FLAGS = "-g"
    #opt.OPTIMISE = "-O3 -fomit-frame-pointer --inline -DNDEBUG"
    opt.OPTIMISE = "-O3 -fomit-frame-pointer --inline"

    # RF: a hack to make the VS build work. sort of an "any last words"
    # argument/hook before link directives.
    opt.PRE_LINK_FLAGS = ""

    super(gnu_mixin, self).detect_model()

    if opt.model in ["cygwin", "win32", "win64"]:
      opt.EXT_EXE = ".exe"
      opt.EXT_SHLIB = ".dll"
      opt.EXT_DYLIB = opt.EXT_SHLIB
      opt.SHLIB_DIR = "bin"

    if opt.model in ["win32", "win64"]:
      opt.EXT_STATIC_OBJ = "_static.obj"
      opt.EXT_SHARED_OBJ = "_dynamic.obj"

    if opt.model == "osx":
      # flags for rtl & flx executable compilation & linking taken from
      # http://fink.sourceforge.net/doc/porting/shared.php with many thanks

      # differentiated now because osx treats dylibs
      # and plugin-type libraries.
      opt.EXT_DYLIB = ".dylib"


  def detect_warning_flags(self):
    opt = self.options

    # find if we have g++ with -Wno-invalid-offsetof
    #
    # NOTE: this is a hack .. Felix makes offsetof() errors
    # We HAVE to detect if the switch turn them off is available
    # first, and if not use -w, otherwise we use -Wall -Wno-offsetof,
    # because these errors must be tolderated in Felix generated C++
    #
    # But the error can't occur in C, and specifying the option
    # causes gcc to barf with a warning
    #
    if self.lang == "g++":
      try:
        self.compile_dummy_main(CFLAGS="-Wno-invalid-offsetof")
        opt.NO_INVALID_OFFSETOF_WARNING = "-Wall -Wno-invalid-offsetof"
        print "-Wno-invalid-offsetof supported"
      except ExecutionError:
        opt.NO_INVALID_OFFSETOF_WARNING = "-w"
    else:
      opt.NO_INVALID_OFFSETOF_WARNING = "-Wall"

    # find if we have g++ with -Wfatal-errors
    try:
      self.compile_dummy_main(CFLAGS="-Wfatal-errors")
      opt.NO_INVALID_OFFSETOF_WARNING = opt.NO_INVALID_OFFSETOF_WARNING + " -Wfatal-errors"
      print "-Wfatal-errors supported"
    except ExecutionError:
      pass

  ########

  def detect_gcc_builtin_expect(self):
    opt = self.options

    filename = self.write_src(r"""
int main(int argc, char** argv) {
   if(__builtin_expect(1,1));
   return 0;
}
""", 'tmp' + os.sep + 'gnu_builtin')

    try:
      self.build_static_program(filename)
      opt.HAVE_GNU_BUILTIN_EXPECT= True
      print "gcc __builtin_expect() support detected"
    except ExecutionError:
      opt.HAVE_GNU_BUILTIN_EXPECT= False

  def detect_named_registers(self):
    opt = self.options

    filename = self.write_src(r"""
#include <stdio.h>
register void *sp __asm__ ("esp");

int main(int argc, char** argv) {
   printf("Sp = %p\n",sp);
   return 0;
}
""", 'tmp' + os.sep + 'gnu_x86')

    # find if we have gnu on 32 bit x86 platform with named registers
    try:
      self.build_static_program(filename)
      opt.HAVE_GNU_X86 = True
      print "gnu x86 32 bit support detected"
    except ExecutionError:
      opt.HAVE_GNU_X86 = False

    filename = self.write_src(r"""
#include <stdio.h>
register void *sp __asm__ ("rsp");

int main(int argc, char** argv) {
   printf("Sp = %p\n",sp);
   return 0;
}
""", 'tmp' + os.sep + 'gnu_x86_64')

    # find if we have gnu on 64 bit x86 platform with named registers
    try:
      self.build_static_program(filename)
      opt.HAVE_GNU_X86_64 = True
      print "gnu x86 64 bit support detected"
    except ExecutionError:
      opt.HAVE_GNU_X86_64 = False

    # X86_64 dominates X86
    if opt.HAVE_GNU_X86 and opt.HAVE_GNU_X86_64:
      opt.HAVE_GNU_X86 = False

    if opt.HAVE_GNU_X86:
      opt.USE_REGPARM3 = True
      print "regparm3 supported"
    else:
      opt.USE_REGPARM3 = False


  def detect_computed_gotos(self):
    opt = self.options

    filename = self.write_src("""
int main(int argc, char** argv) {
  void *label = &&label2;
  goto *label;
  label1:
    return 1;
  label2:
    return 0;
}
""", 'tmp' + os.sep + 'cgoto')


    # find if we have g++ supporting computed jumps
    try:
      self.build_static_program(filename)
      opt.HAVE_CGOTO = True
      print "Computed goto supported"
    except ExecutionError:
      opt.HAVE_CGOTO = False

    filename = self.write_src("""
int main(int argc, char** argv) {
  void *label = &&label2;
  __asm__(".global fred");
  __asm__("fred:");
  __asm__(""::"g"(&&label1));
  goto *label;
  label1:
    return 1;
  label2:
    return 0;
}
""", 'tmp' + os.sep + 'asm_labels')

    # find if we have g++ supporting computed jumps and asm labels
    try:
      self.build_static_program(filename)
      opt.HAVE_ASM_LABELS = True
      print "Asm labels supported"
    except ExecutionError:
      opt.HAVE_ASM_LABELS = False


  def detect_openmp(self):
    # find if we can use -fopenmp without a warning
    try:
      self.compile_dummy_main(CFLAGS="-Werror -fopenmp")
    except ExecutionError:
      print "OpenMP based parallel programming not supported"
    else:
      print "OpenMP based parallel programming supported"

      self.options.HAVE_STATIC_OPENMP = True
      self.options.HAVE_SHARED_OPENMP = True
      self.options.OPENMP = "-fopenmp"


  def detect_PIC(self):
    # find if we can use -fPIC without a warning
    # if a warning is generated it will say something like
    # 'all code is relocatable on this platform'
    # so we make that into an error, detect it, and say -fPIC only
    # if it would not generate this warning
    try:
      self.compile_dummy_main(CFLAGS="-Werror -fPIC")
    except ExecutionError:
      print "All code is position independent"

      self.options.HAVE_PIC = False
    else:
      print "-fPIC supported"

      self.options.HAVE_PIC = True
      self.options.PIC = "-fPIC"

  def construct_compiler_commands(self):
    opt = self.options

    if opt.model == "osx":
      #COMPILE_DYNAMIC_RTL = "-bundle -c"
      COMPILE_DYNAMIC_RTL = opt.SPEC_COMPILE_OBJ + " -fno-common"
      # make a dynamic library (not loadable via APIs like dlcompat)
      LINK_DYNAMIC_RTL = "-dynamiclib"

      COMPILE_DYNAMIC_MAIN = opt.SPEC_COMPILE_OBJ
      LINK_DYNAMIC_MAIN = ""

      COMPILE_DYNAMIC_FLX = opt.SPEC_COMPILE_OBJ + " -bundle -fno-common"
      LINK_DYNAMIC_FLX = "-bundle"
    else:
      COMPILE_DYNAMIC_RTL = opt.SPEC_COMPILE_OBJ
      LINK_DYNAMIC_RTL = "-shared"

      COMPILE_DYNAMIC_MAIN = opt.SPEC_COMPILE_OBJ
      LINK_DYNAMIC_MAIN = ""

      COMPILE_DYNAMIC_FLX = opt.SPEC_COMPILE_OBJ
      LINK_DYNAMIC_FLX = "-shared"

    COM = opt.COM

    opt.CCOBJ_DYNAMIC_FLX = COM + ' ' + COMPILE_DYNAMIC_FLX
    opt.CCLINK_DYNAMIC_FLX = COM + ' ' + LINK_DYNAMIC_FLX

    opt.CCOBJ_DYNAMIC_RTL = COM + ' ' + COMPILE_DYNAMIC_RTL
    opt.CCLINK_DYNAMIC_RTL = COM + ' ' + LINK_DYNAMIC_RTL

    opt.CCOBJ_DYNAMIC_MAIN = COM + ' ' + COMPILE_DYNAMIC_MAIN
    opt.CCLINK_DYNAMIC_MAIN = COM + ' ' + LINK_DYNAMIC_MAIN

    opt.CCOBJ_STATIC_FLX = COM + ' ' + opt.SPEC_COMPILE_OBJ
    opt.CCOBJ_STATIC_RTL = COM + ' ' + opt.SPEC_COMPILE_OBJ
    opt.CCOBJ_STATIC_MAIN = COM + ' ' + opt.SPEC_COMPILE_OBJ

    opt.CCOBJ_DYNAMIC_RTL = opt.CCOBJ_DYNAMIC_RTL + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCOBJ_DYNAMIC_FLX = opt.CCOBJ_DYNAMIC_FLX + ' ' + opt.NO_INVALID_OFFSETOF_WARNING

    opt.CCOBJ_STATIC_RTL = opt.CCOBJ_STATIC_RTL + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCOBJ_STATIC_FLX = opt.CCOBJ_STATIC_FLX + ' ' + opt.NO_INVALID_OFFSETOF_WARNING

    opt.CCLINK_STATIC = COM


  def detect_pthreads(self):
    opt = self.options

    # Note that this pthread fragment actually tests the return value
    # as incorrectly compiled threaded code on solaris links and runs,
    # but returns errors.
    filename = self.write_src("""
#include <pthread.h>

void* start(void* data)
{
  return NULL;
}

int main(int argc, char** argv) {
  pthread_t thr;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  int res = pthread_create(&thr, &attr, start, NULL);
  pthread_attr_destroy(&attr);
  return res;
}
""", 'tmp' + os.sep + 'pthreads')

    # find out how to do pthreads
    opt.HAVE_PTHREADS = False
    opt.PTHREAD_SWITCH = None
    for switch in [' ', '-lpthread ', '-pthread ', '-pthreads ']:
      try:
        lines = self.run_static_program(filename, LDFLAGS=switch)
      except ExecutionError:
        pass
      else:
        opt.HAVE_PTHREADS = True
        opt.PTHREAD_SWITCH = switch
        break

    if opt.HAVE_PTHREADS:
      print "Posix Threads supported with", opt.PTHREAD_SWITCH
    else:
      print "Posix Threads not supported"


  def detect_mmap(self):
    opt = self.options

    opt.HAVE_MMAP = self.check_header_exists('sys/mman.h')

    if not opt.HAVE_MMAP:
      print "mmap not supported"
      return

    print "mmap supported"

    opt.HAVE_MMAP_PROT_EXEC    = self.check_macro_defined('PROT_EXEC',        'sys/mman.h')
    opt.HAVE_MMAP_PROT_READ    = self.check_macro_defined('PROT_READ',        'sys/mman.h')
    opt.HAVE_MMAP_PROT_WRITE   = self.check_macro_defined('PROT_WRITE',       'sys/mman.h')
    opt.HAVE_MMAP_DENYWRITE    = self.check_macro_defined('MAP_DENYWRITE',    'sys/mman.h')
    opt.HAVE_MMAP_ANONYMOUS    = self.check_macro_defined('MAP_ANON',         'sys/mman.h')
    opt.HAVE_MMAP_FILE         = self.check_macro_defined('MAP_FILE',         'sys/mman.h')
    opt.HAVE_MMAP_FIXED        = self.check_macro_defined('MAP_FIXED',        'sys/mman.h')
    opt.HAVE_MMAP_HASSEMAPHORE = self.check_macro_defined('MAP_HASSEMAPHORE', 'sys/mman.h')
    opt.HAVE_MMAP_SHARED       = self.check_macro_defined('MAP_SHARED',       'sys/mman.h')
    opt.HAVE_MMAP_PRIVATE      = self.check_macro_defined('MAP_PRIVATE',      'sys/mman.h')
    opt.HAVE_MMAP_NORESERVE    = self.check_macro_defined('MAP_NORESERVE',    'sys/mman.h')
    opt.HAVE_MMAP_LOCKED       = self.check_macro_defined('MAP_LOCKED',       'sys/mman.h')
    opt.HAVE_MMAP_GROWSDOWN    = self.check_macro_defined('MAP_GROWSDOWN',    'sys/mman.h')
    opt.HAVE_MMAP_32BIT        = self.check_macro_defined('MAP_32BIT',        'sys/mman.h')
    opt.HAVE_MMAP_POPULATE     = self.check_macro_defined('MAP_POPULATE',     'sys/mman.h')
    opt.HAVE_MMAP_NONBLOCK     = self.check_macro_defined('MAP_NONBLOCK',     'sys/mman.h')

    try:
      output = self.run_static_string_program("""
#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
   size_t n = 10000;
   int fd;
   fd = open("/dev/zero", O_RDWR);
   if (fd == -1) {
     return 1;
   }
   void *data =
       mmap
       (
          NULL,n,
          PROT_WRITE | PROT_READ,
          MAP_PRIVATE,
          fd,0
       )
    ;
   if (data == MAP_FAILED)
   {
      return 1;
   }
   int res = munmap(data,n);
   if (res != 0)
   {
      return 1;
   }
   return 0;
}
""", 'tmp'+os.sep+'mmap')
    except ExecutionError:
      pass
    else:
      opt.HAVE_MMAP_DEV_ZERO = True


  def detect_win_dynamic_loading(self):
    opt = self.options

    # check if can get LoadLibrary to work
    basename = 'tmp' + os.sep + 'win32_dummy_lib'
    # RF: This can be compiled as both c and c++ these days it seems
    # hence conditional extern "C"
    dummy_lib_filename = self.write_lib_src("""
#ifdef __cplusplus
extern "C"
#endif
__declspec(dllexport) int fred(int argc,char** argv) { return 0; }
""", basename)

    basename = 'tmp' + os.sep + 'win32_dummy_main'
    dummy_main_filename = self.write_src("""
#include <windows.h>
#include <stdlib.h>

int main(int argc,char** argv) {
   HMODULE lib = LoadLibrary(argv[1]);
   void *fred;
   if(!lib) exit(1);
   fred = (void*)GetProcAddress(lib,"fred");
   if(!fred) exit(1);
   return 0;
}
""", basename)

    try:
      dll = self.build_shared_dll(dummy_lib_filename)
      exe = self.build_shared_program(dummy_main_filename)

      self.shell(exe, dll)
    except ExecutionError:
      pass
    else:
      opt.SUPPORT_DYNAMIC_LOADING = True
      opt.HAVE_LOADLIBRARY = True
      print "Dynamic Loading Supported (with LoadLibrary)"


  def detect_posix_dynamic_loading(self):
    opt = self.options

    basename = 'tmp' + os.sep + 'dummy_lib'
    # RF: This can be compiled as both c and c++ these days it seems
    # hence conditional extern "C"
    # P.S. This lovingly hand crafted function doesn't seem to be called
    proggy = """
#ifdef __cplusplus
extern "C"
#endif
int fred(int argc, char** argv) { return 0; }
"""

    dummy_lib_filename = self.write_lib_src( proggy, basename)

    basename = 'tmp' + os.sep + 'dummy_main'
    dummy_main_filename = self.write_src(r"""
#include <dlfcn.h>
#include <stdlib.h>

int main(int argc, char** argv) {
   void *lib = dlopen(argv[1],RTLD_NOW);
   void *fred = 0;
   if(!lib) exit(1);
   fred = dlsym(lib,"fred");
   if(!fred) exit(1);
   return 0;
}
""", basename)

    try:
      dll = self.build_shared_dll(dummy_lib_filename)
      exe = self.build_shared_program(dummy_main_filename)
      self.shell(exe, dll)
      opt.SUPPORT_DYNAMIC_LOADING = True
      opt.HAVE_DLOPEN = True
    except ExecutionError:
      try: # nope, try with -ldl
        exe = self.build_shared_program([dummy_main_filename], libs=['dl'])
        opt.HAVE_DLOPEN = True
        opt.SUPPORT_DYNAMIC_LOADING = True
        opt.DLLIB = "dl"
        print "Dynamic Loading Supported (with -ldl)"
      except ExecutionError:
        if opt.model == "osx":
          opt.SUPPORT_DYNAMIC_LOADING = True  # pre 10.3, we do our own dlopen

  def detect_dynamic_loading(self):
    opt = self.options

    #check if we can get dlopen to work without -ldl (BSD, Cygwin don't need)
    opt.SUPPORT_DYNAMIC_LOADING = False
    opt.HAVE_DLOPEN = False
    opt.HAVE_LOADLIBRARY = False
    opt.DLLIB = ""

    if opt.model in ["win32", "win64"]:
      self.detect_win_dynamic_loading()
    else:
      self.detect_posix_dynamic_loading()

    if not opt.SUPPORT_DYNAMIC_LOADING:
      print "DYNAMIC LOADING NOT SUPPORTED"
      print "Temporarily this is mandatory [during config debugging]"
      sys.exit(1)


  def detect_sockets(self):
    opt = self.options

    filename = self.write_src(r"""
#include <sys/types.h>
#include <sys/socket.h>
extern "C" int accept(int s, struct sockaddr *addr, socklen_t *addrlen);
int main(int argc, char** argv) { return 0; }
""", 'tmp' + os.sep + 'have_socketlen_t')

    try:
      self.compile_static_main([filename])
      opt.FLX_SOCKLEN_T = "socklen_t"
    except ExecutionError:
      filename = self.write_src(r"""
#include <sys/types.h>
#include <sys/socket.h>
extern "C" int accept(int s, struct sockaddr *addr, unsigned int *addrlen);
int main(int argc, char** argv) { return 0; }
""", 'tmp' + os.sep + 'have_socketlen_t_is_uint')

      try:
        self.compile_static_main([filename])
        opt.FLX_SOCKLEN_T = "unsigned int"
      except ExecutionError:
        filename = self.write_src(r"""
#include <sys/types.h>
#include <sys/socket.h>
extern "C" int accept(int s, struct sockaddr *addr, int *addrlen);
int main(int argc, char** argv) { return 0; }
""", 'tmp' + os.sep + 'have_socketlen_t_is_int')

        try:
          self.compile_static_main([filename])
          opt.FLX_SOCKLEN_T = "int"
        except ExecutionError:
          opt.FLX_SOCKLEN_T = "int"
    print "socklen_t =", opt.FLX_SOCKLEN_T


  def detect_kqueues(self):
    opt = self.options

    filename = self.write_src(r"""
#include <sys/types.h>      // from the kqueue manpage
#include <sys/event.h>      // kernel events
#include <sys/time.h>       // timespec (kevent timeout)

int
main(int argc, char** argv) {
  int kq = kqueue();
  return (-1 == kq) ? 1 : 0;
}
""", 'tmp' + os.sep + 'kqt')

    # see what sort of demuxers we support. right now just testing for
    # kqueues, to unbreak the osx 10.2.8 build. I need demux to be extracted
    # by this point, so I've added a line to configure to do that. That
    # required me to remove the flx_demux.pak's dependence on the config
    # directory (not created at configure time). also had to create a fake
    # flx_rtl_config.h in tmp/

    # now that kqueue demuxer uses condition vars and locks for a clean
    # takedown using this method of config is a nightmare before config
    # because the pthread pak file depends on the config results. for now
    # I've replaced the whole lot with a simple kqueue+main programme.
    try:
      # basically a un*x test, note the non portable path separators and
      # gcc style switches
      self.compile_static_main([filename])
      opt.HAVE_KQUEUE_DEMUXER = True
    except ExecutionError:
      opt.HAVE_KQUEUE_DEMUXER = False

    print "HAVE_KQUEUE_DEMUXER =", opt.HAVE_KQUEUE_DEMUXER

  def detect_epoll(self):
    opt = self.options

    filename = self.write_src(r"""
#include <sys/epoll.h>

int
main(int argc, char** argv) {
  int efd = epoll_create(20);
  return (-1 == efd) ? 1 : 0;
}
""", 'tmp' + os.sep + 'epolltest')

    try:
      lines = self.run_static_program(filename)
    except ExecutionError:
      opt.HAVE_EPOLL = False
    else:
      opt.HAVE_EPOLL = True

    print "HAVE_EPOLL=", opt.HAVE_EPOLL

  def detect_strip(self):
    opt = self.options

    # see if we have strip: it isn't considered essential
    filename = self.compile_dummy_main()
    if opt.STRIP:
      try:
        self.shell(opt.STRIP, filename)
      except:
        opt.STRIP = "true strip"
    else:
      opt.STRIP = "true strip"


  def detect_ar(self):
    opt = self.options

    # see if we have ar
    filenames = self.compile_dummy_main()
    self.shell(opt.AR, os.path.join("tmp", "dummy.a"), *filenames)

    # see if we have ranlib, it isn't considered essential
    # (a totally brain dead Unix idea: AR should do this)
    try:
      self.shell(opt.RANLIB, os.path.join("tmp", "dummy.a"))
    except ExecutionError:
      opt.RANLIB = "true ranlib"


  def detect_compiler_options(self):
    self.detect_warning_flags()
    self.detect_PIC()
    self.construct_compiler_commands()
    self.detect_dynamic_loading()
    self.detect_openmp()


  def check_options(self):
    self.detect_gcc_builtin_expect()
    self.detect_named_registers()
    self.detect_computed_gotos()
    self.detect_pthreads()
    self.detect_mmap()
    self.detect_sockets()
    # could just replace this with header_exists("sys/event.h") but
    # it ain't broke.
    self.detect_kqueues()
    self.options.HAVE_POLL = self.check_header_exists("poll.h")
    self.detect_epoll()
    # nice one, Sun, no one else would ever call a header file "port.h"
    self.options.HAVE_EVTPORTS = self.check_header_exists("port.h")
    print "HAVE_POLL =", self.options.HAVE_POLL
    print "HAVE_EPOLL =", self.options.HAVE_EPOLL
    print "HAVE_EVTPORTS =", self.options.HAVE_EVTPORTS

    self.detect_strip()
    self.detect_ar()

  ####

  def link_thing(self, *args, **kwds):
    opt = self.options

    # strip off the lib from the start of the libraries
    libs = []
    for lib in kwds.get('libs', []):
      if lib[0:3] == 'lib': lib = lib[3:]
      libs.append(lib)
    kwds['libs'] = libs

    if opt.__dict__.get('HAVE_PTHREADS', False):
      if 'LDFLAGS' in kwds:
        kwds['LDFLAGS'] += ' ' + opt.PTHREAD_SWITCH
      else:
        kwds['LDFLAGS'] = opt.PTHREAD_SWITCH

    return super(gnu_mixin, self).link_thing(*args, **kwds)


  def link_shared_thing(self, *args, **kwds):
    if self.options.DLLIB:
      libs = kwds.get('libs', [])[:]
      libs.append(self.options.DLLIB)
      kwds['libs'] = libs

    return super(gnu_mixin, self).link_thing(*args, **kwds)


  def report_config(self):
    super(gnu_mixin, self).report_config()
    opt = self.options

    print
    if opt.SUPPORT_DYNAMIC_LOADING:
      if opt.HAVE_DLOPEN:
        if opt.DLLIB:
          print "Dynamic Loading Supported : with dlopen() in -l", opt.DLLIB
        else:
          print "Dynamic Loading Supported : with dlopen() [native]"
      if opt.HAVE_LOADLIBRARY:
          print "Dynamic Loading Supported : with LoadLibrary"
    else:
      print "Dynamic Loading               : NOT SUPPORTED"
    print

import os
import sys

import fbuild.flxbuild
from fbuild.flxbuild.c_cxx_base import c_cxx_base
from fbuild.flxbuild.flxutil import ExecutionError

class msvc_mixin(object):
  DEFAULT_COM = 'cl'
  DEFAULT_AR = 'lib'

  def set_options(self, **kwds):
    """these options are model dependent and have to
    be supplied by the client"""

    super(msvc_mixin, self).set_options(**kwds)

    opt = self.options

    # RF: add /GR to generate RTTI information, if this isn't on, we get
    # access violations when doing dynamic_casts. Of course, these access
    # violations are thrown as exceptions, which can be caught with
    # catch(std::exception& e). we also don't want the logo
    # OF COURSE now /GR will have no sense if this happens to be a
    # c compiler. let's hope it doesn't error out.
    opt.COM = self.DEFAULT_COM + " /nologo /GR"
    opt.AR = self.DEFAULT_AR + " /nologo"

  ########

  def detect_model(self):
    super(msvc_mixin, self).detect_model()

    if self.options.model == "detect":
      self.options.model = "win32"


  def detect_compiler_options(self):
    self.detect_warning_flags()
    self.construct_compiler_commands()
    self.detect_openmp()


  # CHECK_OPTIONS FOR CL.EXE IN CASE YOU WERE WONDERING
  # RIGHT NOW WE'RE IN THE FILE msvc_mixin.py, SOMETIMES THAT CAN
  # BE CONFUSING.
  def check_options(self):
    opt = self.options
    COM = opt.COM

    # RF: Does windows have mmap? Let's say no.
    opt.HAVE_MMAP = False
    opt.RANLIB = "@rem nothing" # RF: better than none.
    opt.SUPPORT_DYNAMIC_LOADING = True
    opt.SPEC_COMPILE_OBJ = "/c"
    opt.SPEC_OBJ_FILENAME = "/Fo"
    opt.SPEC_EXE_FILENAME = "/Fe"
    opt.SPEC_DEFINE = "/D"
    opt.SPEC_INCLUDE = "/I"
    opt.SPEC_LIBPATH = "/LIBPATH:"
    opt.SPEC_LIB = "/DEFAULTLIB:"
    opt.SPEC_AR_OUT_FILENAME = "/OUT:"
    #opt.DEBUG_FLAGS = "/Yd /Zi /RTC"
    # /Yd deprecated, /RTC not recognized with VS2005
    opt.DEBUG_FLAGS = "/Zi"
    #opt.OPTIMISE = "/Ox /DNDEBUG"
    opt.OPTIMISE = "/Ox"

    opt.EXT_LIB = ".lib"
    opt.EXT_EXE= ".exe"
    opt.EXT_SHLIB = ".dll"
    opt.EXT_DYLIB = opt.EXT_SHLIB
    opt.EXT_STATIC_OBJ = "_static.obj"
    opt.EXT_SHARED_OBJ = "_dynamic.obj"

    opt.HAVE_DLOPEN = False
    opt.DLLIB = ""

    opt.FLX_SOCKLEN_T = 'int'
    opt.HAVE_PTHREADS = False
    opt.HAVE_KQUEUE_DEMUXER = False
    opt.HAVE_POLL = False
    opt.HAVE_EPOLL = False
    opt.HAVE_EVTPORTS = False

    # where to put the rtl: Cygwin requires the dll be in the PATH
    opt.SHLIB_DIR = "bin"
    print "rtl located in bin directory"

  def detect_openmp(self):
    # find if we can use /openmp without a warning
    try:
      self.run_shared_string_program(
        'int main(int argc, char** argv) { return 0; }',
        os.path.join('tmp', 'openmp'), CFLAGS='/openmp')
    except ExecutionError:
      print "OpenMP based parallel programming not supported"
    else:
      print "OpenMP based parallel programming supported"

      self.options.HAVE_SHARED_OPENMP = True
      self.options.OPENMP = "/openmp"


  def detect_warning_flags(self):
    # RF: silencing all warnings /w has masked some really insidious bugs
    # (like dynamic_casts whilst RTTI was disabled). does the offset warning
    # even apply to vs toolchain? I don't think so, however, there is one
    # warning, that about missing delete for the custom operator new,
    # the follow /wd<n> should silence that warning.
    # self.options.NO_INVALID_OFFSETOF_WARNING = "/w"
    self.options.NO_INVALID_OFFSETOF_WARNING="/wd4291"


  def construct_compiler_commands(self):
    opt = self.options
    COM = opt.COM
    # RF: /MT (link with multithreaded clib, LIBCMT.LIB) for static builds
    # and /MD for dynamic (multithreaded dynamic clib, MSVCRT.LIB).
    # This last one's important as it means that not only is malloc threadsafe,
    # but the SAME allocator is shared between the app and its dynamic libs.
    # Without this, pushing an fthread in flx_run and popping it in
    # flxdynamic_lib is actually an insidious error. Note that /M* flags are
    # not just for link time, they seem to need to be passed to the compilation
    # phase as well. To run with debug versions of clib, try /MDd, /MTd and
    # /LDd when linking dynamic libs. Phew.
    # RF: update: Max suggested trying /MD (threadsafe dll clib) for all
    # builds, including static. I forget why, perhaps for uniformity. Anyway,
    # it works fine. It might have implications for folks who link against
    # static libs (folks like me), but I'm using nocygwin, so, yknow, eh.
    # P.S. Erick, if ever again you checkin any changes to this that you
    # haven't first tested, I'll kill you.

    COMPILE_DYNAMIC_RTL = "/MD /c /EHs"
    LINK_DYNAMIC_RTL = "/MD /LD"

    COMPILE_DYNAMIC_MAIN = "/MD /c /EHs"
    LINK_DYNAMIC_MAIN = "/MD"

    COMPILE_DYNAMIC_FLX = "/MD /c /EHs"
    LINK_DYNAMIC_FLX = "/MD /LD"

    opt.CCOBJ_DYNAMIC_FLX = COM + ' ' + COMPILE_DYNAMIC_FLX
    opt.CCLINK_DYNAMIC_FLX = COM + ' ' + LINK_DYNAMIC_FLX

    opt.CCOBJ_DYNAMIC_RTL = COM + ' ' + COMPILE_DYNAMIC_RTL
    opt.CCLINK_DYNAMIC_RTL = COM + ' ' + LINK_DYNAMIC_RTL

    opt.CCOBJ_DYNAMIC_MAIN = COM + ' ' + COMPILE_DYNAMIC_MAIN
    opt.CCLINK_DYNAMIC_MAIN = COM + ' ' + LINK_DYNAMIC_MAIN

    opt.CCOBJ_STATIC_FLX = COM + " /MD /c /EHs"
    opt.CCOBJ_STATIC_RTL = COM + " /MD /c /EHs"
    opt.CCOBJ_STATIC_MAIN = COM + " /MD /c /EHs"

    opt.CCOBJ_DYNAMIC_RTL = opt.CCOBJ_DYNAMIC_RTL + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCOBJ_DYNAMIC_FLX = opt.CCOBJ_DYNAMIC_FLX + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCOBJ_STATIC_RTL = opt.CCOBJ_STATIC_RTL + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCOBJ_STATIC_FLX = opt.CCOBJ_STATIC_FLX + ' ' + opt.NO_INVALID_OFFSETOF_WARNING
    opt.CCLINK_STATIC = COM + " /MD"

  ########

  def link_thing(self, *args, **kwds):
    kwds['LDFLAGS'] = kwds.get('LDFLAGS', '')
    # RF: hack to get /link before all other link directives,
    # including the libraries themselves and their paths.
    self.options.PRE_LINK_FLAGS = '/link'

    return super(msvc_mixin, self).link_thing(*args, **kwds)

  ########

  def report_config(self):
    c_cxx_base.report_config(self)
    opt = self.options

    print
    if opt.SUPPORT_DYNAMIC_LOADING:
      print "Dynamic Loading Supported               : [Windows native]"
    else:
      print "Dynamic Loading                         : NOT SUPPORTED"

    print
    self.report_isnan()

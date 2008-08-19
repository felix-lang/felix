import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs

import config

class build_felix_dynamic_drivers(Process):
  def runme(self, pkg, pkgdict, *args):
    if not config.SUPPORT_DYNAMIC_LOADING:
      return

    DRIVERS = pkgdict.get("dynamic_drivers",[])

    if not DRIVERS:
      return

    print "COMPILING DRIVERS (dynamic)"
    cflags = pkgdict.get("cflags","")
    dflags = pkgdict.get("dflags","")
    LIBS = pkgdict.get("drivers_require_libs",[])
    libs = []
    for lib in LIBS:
      libs.append(lib+"_dynamic")

    for src, outdir in DRIVERS:
      # this is a gross HACK! Don't make dynamic versions
      # of drivers with extra macros, they're for static
      # link of driver without async support
      print 'dynamic Compiling driver object', src

      dst = config.TARGET_CXX.build_shared_program(src, os.path.join('build', src),
          outdir='build',
          include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
          optimise=self.optimise,
          debug=self.debug,
          CFLAGS=cflags,
          lib_paths=[config.TARGET_CXX.options.SHLIB_DIR],
          libs=libs,
          LDFLAGS=dflags)

      if outdir:
        print 'copying file', dst, '->', outdir
        mkdirs(outdir)
        shutil.copy(dst, outdir)

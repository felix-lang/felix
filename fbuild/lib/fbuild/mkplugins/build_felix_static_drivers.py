import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs

import config

class build_felix_static_drivers(Process):
  def runme(self, pkg, pkgdict, *args):
    if not config.SUPPORT_STATIC_LINKAGE:
      return

    DRIVERS = pkgdict.get("static_drivers",[])
    if not DRIVERS:
      return

    print "COMPILING DRIVERS (static)"
    cflags = pkgdict.get("cflags","")

    for src, outdir in DRIVERS:
      print 'static Compiling driver object', src

      dst = config.TARGET_CXX.compile_static_main([src],
        outdir='build',
        include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
        macros=["FLX_STATIC_LINK"],
        optimise=self.optimise,
        debug=self.debug,
        CFLAGS=cflags)

      if outdir:
        print 'copying file', dst[0], '->', outdir
        mkdirs(outdir)
        shutil.copy(dst[0], outdir)

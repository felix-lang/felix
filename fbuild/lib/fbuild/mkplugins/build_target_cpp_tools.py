import os

from fbuild.flxbuild.process import Process

import config

class build_target_cpp_tools(Process):
  def runme(self, pkg, pkgdict, *args):
    EXES = pkgdict.get("exes",[])
    esflags = pkgdict.get("exes_require_linkflags","")
    if len(EXES)>0:
      print "BUILDING C++ TARGET TOOLS"
      LIBS = pkgdict.get("exes_require_libs",[])
      libs = []
      for lib in LIBS: libs.append(lib+"_static")
      for src,bin in EXES:
        dir = os.path.dirname(src)
        config.TARGET_CXX.build_static_main(src, dir, bin,
          outdir='build',
          include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
          macros=["FLX_STATIC_LINK"],
          lib_paths=[dir],
          libs=libs,
          LDFLAGS=esflags)

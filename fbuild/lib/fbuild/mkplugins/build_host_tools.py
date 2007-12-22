import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native, mkdirs

import config

class build_host_tools(Process):
  def copy_hpp2rtl(self, HPPS):
    for path in HPPS:
      f = os.path.join(config.src_dir, path)
      if os.path.exists(f):
        src = f
      else:
        src = path
      src = unix2native(src)
      dst = os.path.join(config.FLX_RTL_DIR, os.path.basename(path))

      if not self.quiet: print 'copying file', src, '->', dst
      mkdirs(os.path.dirname(dst))
      shutil.copyfile(src, dst)


  def runme(self, pkg, pkgdict, *args):
    HPPS = pkgdict.get("rtl_interfaces",[])
    self.copy_hpp2rtl(HPPS)

    CCS = pkgdict.get("host_cc_ccs",[])
    CPPS = pkgdict.get("host_cpp_cpps",[])
    EXES = pkgdict.get("host_exes",[])
    LIBS = pkgdict.get("host_exes_require_libs",[])
    MACROS =pkgdict.get("host_macros",[])
    INCLUDES=pkgdict.get("host_include_path",[])
    INCLUDE_PATH=[os.path.join(config.src_dir, i) for i in INCLUDES]

    if len(CCS)+len(CPPS)+len(EXES)+len(LIBS) == 0:
      return

    print "BUILDING HOST TOOLS"
    pkglib = None

    if CCS:
      print "HOST C COMPILING", pkg
      pkglib = "lib"+pkg+"_host_static"

      config.HOST_C.build_static_rtl(CCS, os.path.join('build', 'hostlib', pkglib),
        outdir='build',
        include_paths=INCLUDE_PATH+[config.FLX_RTL_DIR, config.FLX_HOST_CONFIG_DIR, "elk"],
        macros=MACROS+["FLX_STATIC_LINK"],
      )

    if CPPS:
      print "HOST C++ COMPILING", pkg
      pkglib = "lib"+pkg+"_host_static"

      config.HOST_CXX.build_static_rtl(CPPS, os.path.join('build', 'hostlib', pkglib),
        outdir='build',
        include_paths=INCLUDE_PATH+[config.FLX_RTL_DIR, config.FLX_HOST_CONFIG_DIR, "elk"],
        macros=MACROS+["FLX_STATIC_LINK"],
      )

    for x in CCS+CPPS:
      f = x + config.HOST_CXX.options.EXT_STATIC_OBJ

    kwds = {\
      'include_paths': INCLUDE_PATH+[config.FLX_RTL_DIR, config.FLX_HOST_CONFIG_DIR, "elk"],
      'macros': MACROS+["FLX_STATIC_LINK"],
    }

    if pkglib:
      kwds['lib_paths'] = [os.path.join('build', 'hostlib')]
      kwds['libs'] = [pkglib]

    for src, bin in EXES:
      config.HOST_CXX.build_static_program(src, bin, **kwds)

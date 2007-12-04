import os

from fbuild.flxbuild.process import Process

import config

class build_target_rtl_static(Process):
  def runme(self, pkg, pkgdict, *args):
    if not config.SUPPORT_STATIC_LINKAGE:
      return

    CCS = pkgdict.get("cc_ccs",[])
    CPPS = pkgdict.get("cpp_cpps",[])

    if not CCS and not CPPS:
      return

    EXTRA_CFLAGS = pkgdict.get("cflags","")
    lib = pkgdict.get("provides_lib","lib"+pkg)
    INCLUDES=pkgdict.get("include_path",[])
    INCLUDE_PATH=[os.path.join(config.FLX_DIR, i) for i in INCLUDES]
    MACROS =pkgdict.get("macros",[])

    print " ++ "+pkg+" RTL (static)"

    kwds = {
        'outfile': os.path.join(config.FLX_RTL_DIR, lib + "_static"),
        'outdir': 'build',
        'include_paths': INCLUDE_PATH+[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
        'macros': MACROS+["FLX_STATIC_LINK"],
        'optimise': self.optimise,
        'debug': self.debug,
        'CFLAGS': EXTRA_CFLAGS,
        }

    if CCS:
      config.TARGET_CC.build_static_rtl(CCS, **kwds)

    if CPPS:
      config.TARGET_CXX.build_static_rtl(CPPS, **kwds)

    for x in CCS + CPPS:
      f = x + config.TARGET_CXX.options.EXT_STATIC_OBJ

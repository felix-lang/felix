import os

from fbuild.flxbuild.process import Process

import config

class build_target_rtl_dynamic(Process):
  def runme(self, pkg, pkgdict, *args):
    if not config.SUPPORT_DYNAMIC_LOADING:
      return

    CCS = pkgdict.get("cc_ccs",[])
    CPPS = pkgdict.get("cpp_cpps",[])

    if not CCS and not CPPS:
      return

    BUILD_MACRO = pkgdict.get("build_macro","ERROR!")
    EXTRA_CFLAGS = pkgdict.get("cflags","")
    EXTRA_DFLAGS = pkgdict.get("dflags","")
    INCLUDES=pkgdict.get("include_path",[])
    INCLUDE_PATH=[os.path.join(config.FLX_DIR, i) for i in INCLUDES]
    MACROS =pkgdict.get("macros",[])

    flibs = pkgdict.get("lib_requires",[])
    needs_libs = []
    for i in flibs:
      needs_libs.append(i+"_dynamic")

    lib = pkgdict.get("provides_lib","lib"+pkg)

    print " ++ "+pkg+" RTL (dynamic)"

    compile_kwds = {
        'outdir': 'build',
        'include_paths': INCLUDE_PATH+[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
        'optimise': self.optimise,
        'debug': self.debug,
        'macros': MACROS+["BUILD_"+BUILD_MACRO],
        'CFLAGS': EXTRA_CFLAGS,
        }

    link_kwds = {
        'lib_paths': [config.TARGET_CXX.options.SHLIB_DIR],
        'libs': needs_libs,
        'LDFLAGS': EXTRA_DFLAGS,
        }

    if CCS:
      # RF: THIS SHOULD BE TARGET_CC AND THE OUTPUT DIR SHOULD BE THAT OF
      # THE TARGET.
      objects = config.TARGET_CC.compile_shared_rtl(CCS, **compile_kwds)
      library = config.TARGET_CC.options.SHLIB_DIR+os.sep+lib+'_dynamic'

      config.TARGET_CC.link_shared_rtl(objects, library, **link_kwds)

      if config.TARGET_CC.options.EXT_DYLIB != config.TARGET_CC.options.EXT_SHLIB:
        config.TARGET_CC.link_shared_dll(objects, library,
          **link_kwds)

    if CPPS:
      # RF: THIS SHOULD BE TARGET_CXX AND THE OUTPUT DIR SHOULD BE THAT OF
      # THE TARGET.
      objects = config.TARGET_CXX.compile_shared_rtl(CPPS, **compile_kwds)
      library = config.TARGET_CXX.options.SHLIB_DIR+os.sep+lib+'_dynamic'

      config.TARGET_CXX.link_shared_rtl(objects, library, **link_kwds)

      if config.TARGET_CXX.options.EXT_DYLIB != config.TARGET_CXX.options.EXT_SHLIB:
        config.TARGET_CXX.link_shared_dll(objects, library, **link_kwds)

      for x in CCS + CPPS:
        f = x + config.TARGET_CXX.options.EXT_SHARED_OBJ

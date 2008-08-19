import os

from fbuild.flxbuild.process import Process

import config

class build_target_felix_tools(Process):
  def runme(self, pkg, pkgdict, *args):
    flxs = pkgdict.get("felix_tools",[])
    if not flxs:
      return

    print "BUILDING FELIX TARGET TOOLS"
    libs = [l+'_static' for l in pkgdict.get("exes_require_libs",[])]
    fsflags = pkgdict.get("felix_requires_linkflags","")
    for src,exe in flxs:
      # added 'std' here so flx_pkgconfig builds
      self.shell(os.path.join('bin', 'flxg'),
        '-Ilib',
        '--elkhound=' + os.path.join('bin', 'flx_elkhound'),
        '--import=flx.flxh',
        'std',
        src,
      )
      config.TARGET_CXX.build_felix_static(src, exe,
        objects=[os.path.join(config.FLX_RTL_DIR, 'flx_run' + config.TARGET_CXX.options.EXT_STATIC_OBJ)],
        include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
        macros=["FLX_STATIC_LINK"],
        lib_paths=[config.FLX_RTL_DIR],
        libs=libs,
        LDFLAGS=fsflags
      )

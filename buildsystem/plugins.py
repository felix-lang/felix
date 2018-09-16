
import fbuild
import fbuild.builders.file
import os
from fbuild.path import Path
from fbuild.builders.file import copy

# ------------------------------------------------------------------------------

def build(phase, felix):
    print ("BUILDING PLUGINS")
    plugins = [
     "toolchain_clang_macosx",
      "toolchain_clang_linux",
      "toolchain_gcc_macosx",
      "toolchain_gcc_linux",
      "toolchain_msvc_win",
      "toolchain_iphoneos",
      "toolchain_iphonesimulator",
      "flx_plugin",
      ]
    for base in plugins:
      shlib = felix.compile(
        phase.ctx.buildroot/('share/lib/plugins/'+base+'.flx'),
        flags=['-od',phase.ctx.buildroot/'host/lib/rtl'])


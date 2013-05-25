import fbuild
import fbuild.builders.file
import os
from fbuild.path import Path
from fbuild.builders.file import copy

# ------------------------------------------------------------------------------

def build(phase, felix):
    print("BUILDING TOOLS")
    
    # The wiki is killing the compiler, inlining procedure
    # cannot cope. Possible inline explosion.
    # Need to investigate this. Maybe its too big,
    # and maybe the small change I just introduced was enough
    # to kill it.

    #try:
    #    exe = felix.compile(phase.ctx.buildroot/'wiki/wiki.flx', static=True)
    #    fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    #except:
    #    print("Warning : wiki not built. Continuing..." )

    exes = [
      "flx_grep",
      "flx_replace",
      "flx_ls",
      "flx_cp",
      "webserver",
      "flx_gramdoc",
      "flx_libcontents",
      "flx_libindex",
      "flx_renumber",
      "flx_mktutindex",
      "flx_perror",
      "flx_tangle",
      "flx_gengraph",
      "flx_testpack",
      "flx_build_flxg",
      "flx_build_rtl",
      ]

    optional_exes = [
      "norK",
      ]

    for base in exes:
      exe = felix.compile(phase.ctx.buildroot/('share/tools/'+base+'.flx'), 
        static=True,
        includes=[
          phase.ctx.buildroot/'host'/'lib'/'rtl',
          phase.ctx.buildroot/'share'/'lib'/'rtl',
          ])
      fbuild.builders.file.copy(phase.ctx, exe, 'host/bin')
      #os.unlink(exe)

    for base in optional_exes:
      try:
          exe = felix.compile(phase.ctx.buildroot/('tools/'+base+'.flx'), 
            static=True,
            includes=[
              phase.ctx.buildroot/'host'/'lib'/'rtl',
              phase.ctx.buildroot/'share'/'lib'/'rtl',
              ])
          fbuild.builders.file.copy(phase.ctx, exe, 'host/bin')
      except:
          print("Warning : "+base+" not built. Continuing..." )



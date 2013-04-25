import fbuild
import fbuild.builders.file
import os
from fbuild.path import Path
from fbuild.builders.file import copy

# ------------------------------------------------------------------------------

def build(phase, felix):

    exes = [
      "scoop",
      ]

    optional_exes = [
      ]

    for base in exes:
      exe = felix.compile(phase.ctx.buildroot/('share/pkgtool/'+base+'.flx'), 
        static=True,
        #includes=[phase.ctx.buildroot/'share/lib']
        )
      fbuild.builders.file.copy(phase.ctx, exe, 'host/bin')
      os.unlink(exe)
        
    for base in optional_exes:
      try:
          exe = felix.compile(phase.ctx.buildroot/('share/pkgtool/'+base+'.flx'), 
            static=True,
            #includes=[phase.ctx.buildroot/'share/lib']
            )
          fbuild.builders.file.copy(phase.ctx, exe, 'host/bin')
          os.unlink(exe)
      except:
          print("Warning : "+base+" not built. Continuing..." )






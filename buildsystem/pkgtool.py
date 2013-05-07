import fbuild
import fbuild.builders.file
import os
from fbuild.path import Path
from fbuild.builders.file import copy

# ------------------------------------------------------------------------------

def build(phase, felix):
      exe = felix.compile(phase.ctx.buildroot/('share/src/tools/scoop.flx'), 
        static=True,
        )
      fbuild.builders.file.copy(phase.ctx, exe, 'host/bin')
      os.unlink(exe)
        


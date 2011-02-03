import fbuild
from fbuild.path import Path
from fbuild.record import Record
from fbuild.functools import call

import buildsystem

# ------------------------------------------------------------------------------

def build(phase):
    # make flx command line harness
    flx = call('fbuild.builders.felix.Flx', phase.ctx,
          exe=phase.ctx.buildroot / 'bin/flx',
          debug=phase.ctx.options.debug,
          flags=['-c','--test=' + phase.ctx.buildroot])

    path =phase.ctx.buildroot/'tools'
    ls = path / 'flx_ls'

    return Record (
        flx_ls = flx(ls,static=True)
    )

 

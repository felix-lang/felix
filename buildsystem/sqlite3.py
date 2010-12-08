import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    buildsystem.copy_to(
      phase.ctx, 
      phase.ctx.buildroot/'lib/sqlite3',
      Path('src/sqlite3/*.flx').glob())


import os
import sys
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs

import config

class speed_tests(Process):
  help = 'Run comparative tests'

  def __init__(self, *args, **kwds):
    super(speed_tests, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if self.ran:
      return
    self.ran = True

    flx_perf = os.path.join(config.FLX_LPARCHIVE, 'flx_perf.pak')
    self.shell(config.ISCR, flx_perf)

    import speed.measure
    import speed.panal

    self.shell('gnuplot', os.path.join('speed', 'mkjpgs.gpl'))
    self.shell(config.ISCR,
      '--inhibit-sref=1',
      '--language=en',
      '--weaver=web',
      '--passes=2',
      '--weaver-directory=speed/',
      flx_perf,
    )

    shutil.copy(os.path.join('misc', 'interscript.css'), 'speed')

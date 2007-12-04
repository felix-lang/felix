import os
import sys

from fbuild.flxbuild.process import Process

class pfcount(Process):
  def __init__(self, *args, **kwds):
    super(pfcount, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict,*args):
    if self.ran:
      return
    self.ran = True

    # requires posix
    if os.name == 'nt':
      return

    self.shell(sys.executable,
      os.path.join('script', 'pfcount.py'),
      os.path.join('misc', 'fcounts.stats'),
    )

import os
import sys

from fbuild.flxbuild.process import Process

class fcount(Process):
  def __init__(self, *args, **kwds):
    super(fcount, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict,*args):
    if self.ran:
      return
    self.ran = True

    # requires posix
    if os.name == 'nt':
      return

    if "inline" in self.options:
      tkind = "inline"
    elif "noinline" in self.options:
      tkind = "noinline"
    else:
      tkind = "std"

    self.shell(sys.executable,
      os.path.join('script', 'fcount.py'),
      os.path.join('misc', 'fcounts.stats'),
      tkind,
      '"' + os.path.join('tut', 'examples', '*.hpp') + '"',
    )

    self.shell(sys.executable,
      os.path.join('script', 'fcount.py'),
      os.path.join('misc', 'fcounts.stats'),
      tkind,
      '"' + os.path.join('test', '*.hpp') + '"',
    )

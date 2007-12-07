import os
import glob

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import erasefile

import config

# cleans products, but not extracted sources
class clean(Process):
  help = 'remove generated C++ and binaries from test locations'

  def runme(self, *args):
    for d in glob.glob(os.path.join("pkg-stamps", "*")):
      erasefile(d)
    for f in glob.glob(os.path.join(config.FLX_LPARCHIVE, "lpsrc", "*.cache")):
      erasefile(f)
    for f in glob.glob(os.path.join(config.FLX_LPARCHIVE, "lpsrc-cache", "*.cache")):
      erasefile(f)
      

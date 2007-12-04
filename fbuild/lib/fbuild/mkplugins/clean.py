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

    for d in [
      os.path.join("tut", "examples"),
      os.path.join("bagley", "felix"),
      "src","elk","demux","faio","lib","lpsrc","rtl","bin","test",
      ]:
      for e in ["*.hpp","*.so","*.dll","*.cpp","*.hpp",
        "*.par","*.output","*.o","*.obj",
        "*.exp","*.lib",'*.resh','*.par',
        "*.cache","*.a","*.exp","*.exe",
        "*.cmo","*.cma","*.cmi","*.cmx","*.cmxa",
        ]:
        for f in glob.glob(os.path.join(d, e)):
          erasefile(f)

    for f in glob.glob(os.path.join("lib", "*.par")):
      erasefile(f)

    for f in glob.glob(os.path.join(config.FLX_LPARCHIVE, "lpsrc", "*.cache")):
      erasefile(f)

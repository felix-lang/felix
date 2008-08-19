import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native

import config

class extract_iscr(Process):
  def runme(self, pkg, pkgdict, *args):
    srcs = pkgdict.get("iscr_source",[])
    for src in srcs:
      src = unix2native(src)
      if not self.quiet: print "REExtracting", pkg, "from", src
      self.shell(config.ISCR,
        '--break-on-error',
        os.path.join(config.FLX_LPARCHIVE, src),
      )

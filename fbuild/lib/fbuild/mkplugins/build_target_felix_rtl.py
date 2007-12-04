import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native

import config

class build_target_felix_rtl(Process):
  def runme(self, pkg, pkgdict, *args):
    flxs = pkgdict.get('felix_rtl', [])
    if not flxs:
      return

    for path in flxs:
      f = os.path.join(config.FLX_DIR, path)
      if os.path.exists(f):
        src = f
      else:
        src = path
      src = unix2native(src)
      dst = os.path.join('lib', os.path.basename(path))

      if not self.quiet: print 'copying file', src, '->', dst
      shutil.copyfile(src, dst)

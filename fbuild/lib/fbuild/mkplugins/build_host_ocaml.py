import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import unix2native

class copy_mli2ml(Process):
  def runme(self, pkg, pkgdict, *args):
    for f in pkgdict.get('caml_raw_interfaces', []):
      f = unix2native(f)
      src = f+'.mli'
      dst = f+'.ml'
      if not self.quiet: print 'copying file', src, '->', dst
      shutil.copyfile(src, dst)

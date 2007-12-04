import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs, ExecutionError

class typeclassdoc(Process):
  help = 'make the typeclass documentation'

  def __init__(self, *args, **kwds):
    super(typeclassdoc, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if self.ran:
      return
    self.ran = True

    print "GENERATING TYPECLASS and MODULE DOCUMENTATION"
    mkdirs(os.path.join('doc', 'moduledoc'))
    shutil.copy(os.path.join('misc', 'plus.gif'),  os.path.join("doc", "moduledoc"))
    shutil.copy(os.path.join('misc', 'minus.gif'), os.path.join("doc", "moduledoc"))
    shutil.copy(os.path.join('misc', 'dot.gif'),   os.path.join("doc", "moduledoc"))

    try:
      self.shell(os.path.join('bin', 'flxd'),
        '-Ilib',
        '--import=flx.flxh',
        '--document-typeclass',
        os.path.join('misc', 'ldoc'),
      )
    except ExecutionError:
      pass

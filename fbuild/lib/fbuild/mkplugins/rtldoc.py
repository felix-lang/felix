import os
from fbuild.flxbuild.flxutil import mkdirs
from fbuild.flxbuild.process import Process

class rtldoc(Process):
  help = 'make the runtime language documentation'

  def __init__(self, *args, **kwds):
    super(rtldoc, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if self.ran:
      return
    self.ran = True

    mkdirs(os.path.join("doc","rtl"))
    print "GENERATING RTLDOCS"
    try:
      self.shell('doxygen', os.path.join('misc', 'doxconf.dox'))
    except:
      pass

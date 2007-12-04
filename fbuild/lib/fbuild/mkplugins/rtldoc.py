import os

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

    print "GENERATING RTLDOCS"
    try:
      self.shell('doxygen', os.path.join('misc', 'doxconf.dox'))
    except:
      pass

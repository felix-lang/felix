import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import ExecutionError

class gramdoc(Process):
  help = 'make the syntax documentation'

  def __init__(self, *args, **kwds):
    super(gramdoc, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if self.ran:
      return
    self.ran = True

    print "GENERATING SYNTAX DOCUMENTATION"
    f = open(os.path.join('tmp', 'xx.flx'), 'w')
    f.write("#import <nugram.flxh>\n")
    f.write("open syntax felix\n")
    f.close()

    try:
      self.shell(os.path.join('bin', 'flxp'),
        '-Ilib',
        '--document-grammar',
        os.path.join('tmp', 'xx'),
      )
    except ExecutionError:
      pass

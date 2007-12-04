import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import erasedir, ExecutionError

class impldoc(Process):
  help = 'make the ocaml compiler documentation'

  def __init__(self, *args, **kwds):
    super(impldoc, self).__init__(*args, **kwds)
    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if self.ran:
      return
    self.ran = True

    print "GENERATING OCAMLDOCS"
    erasedir(os.path.join('doc', 'impldoc'))
    os.mkdir(os.path.join('doc', 'impldoc'))
    try:
      self.shell('ocamldoc', '-html',
        '-I', 'src',
        '-d', os.path.join('doc', 'impldoc'),
        os.path.join('src', '*.mli'),
      )
      self.shell('ocamldoc', '-latex',
        '-I', 'src',
        '-d', os.path.join('doc', 'impldoc', 'flx_impl.tex'),
        os.path.join('src', '*.mli'),
      )
      self.shell('(cd doc/impldoc; latex --interaction=batchmode flx_impl.tex && latex --interaction=batchmode flx_impl.tex && latex --interaction=batchmode flx_impl.tex)')
    except ExecutionError:
     pass # well ocamldoc is full of bugs ..

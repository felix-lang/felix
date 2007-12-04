import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs

import config

class build_ocamlbuild(Process):
  def runme(self, pkg, pkgdict, *args):
    EXES = pkgdict.get('caml_exes', [])

    for exe in EXES:
      if BYTECODE:
        bin = exe + '.byte'
      else:
        bin = exe + '.native'

      self.shell(config.OCAMLBUILD, bin)

      mkdirs('bin')
      shutil.copy(os.path.join('_build', bin), 'bin')

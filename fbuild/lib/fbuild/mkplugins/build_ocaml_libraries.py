import os

from fbuild.flxbuild.process import Process

import config

class build_ocaml_libraries(Process):
  def runme(self, pkg, pkgdict, *args):
    IMPLEMENTATIONS = [os.path.join('build', f) for f in pkgdict.get("caml_implementations", [])]
    lib = pkgdict.get('caml_provide_lib', os.path.join('src', pkg + 'lib'))

    if not IMPLEMENTATIONS:
      return

    print "CAML CREATING LIBRARY", lib
    config.HOST_OCAML.link_lib(IMPLEMENTATIONS, lib,
      bytecode='bytecode' in self.options,
      outdir='build',
    )

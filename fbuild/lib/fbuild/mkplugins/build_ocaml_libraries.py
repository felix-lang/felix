from fbuild.flxbuild.process import Process

import config

class build_ocaml_libraries(Process):
  def runme(self, IMPLEMENTATIONS, lib):
    if not IMPLEMENTATIONS:
      return

    print "CAML CREATING LIBRARY", lib
    return config.HOST_OCAML.link_lib(IMPLEMENTATIONS, lib,
      bytecode='bytecode' in self.options,
      outdir='build',
    )

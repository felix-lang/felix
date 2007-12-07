import os

from fbuild.flxbuild.process import Process

import config

class build_ocaml_modules(Process):
  def runme(self, pkg, pkgdict, *args):
    INTERFACES = pkgdict.get("caml_interfaces", [])
    IMPLEMENTATIONS = pkgdict.get("caml_implementations", [])
    PACKS = pkgdict.get("caml_pack", [])
    INCLUDES = pkgdict.get("caml_include_paths", [])

    if not (INTERFACES or IMPLEMENTATIONS):
      return

    print "CAML COMPILING", pkg

    kwds = dict(
      outdir='build',
      include_paths=[os.path.join('build', i) for i in INCLUDES],
      packs=PACKS,
      debug=self.debug,
      profile='profile' in self.options,
      optimise='optimise_felix' in self.options,
    )

    config.HOST_OCAML.compile_interface(INTERFACES, **kwds)

    config.HOST_OCAML.compile_module(IMPLEMENTATIONS,
      bytecode='bytecode' in self.options,
      **kwds)

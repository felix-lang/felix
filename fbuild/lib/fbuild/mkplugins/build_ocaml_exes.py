import os
import shutil

from fbuild.flxbuild.process import Process

import config

class build_ocaml_exes(Process):
  def runme(self, pkg, pkgdict, *args):
    EXES = pkgdict.get("caml_exes", [])
    OLIBRARIES = pkgdict.get("caml_require_libs", [])
    INCLUDES = pkgdict.get("caml_include_paths", [])

    if not EXES:
      return

    print "CAML LINKING EXECUTABLES"

    kwds = dict(
      outdir='build',
      bytecode='bytecode' in self.options,
      include_paths=[os.path.join('build', i) for i in INCLUDES],
    )

    output_exes = []
    for exe in EXES:
      config.HOST_OCAML.compile_module([exe], **kwds)

      src = config.HOST_OCAML.link_exe(
        [os.path.join('build', exe)],
        os.path.splitext(exe)[0] + config.HOST_OCAML.options.EXT_EXE,
        libs=OLIBRARIES,
        **kwds)

      dst = os.path.join('bin', os.path.basename(src))

      if not self.quiet: print 'copying file', src, '->', dst
      shutil.copy(src, dst)
      output_exes.append(dst)

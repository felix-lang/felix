import os
import shutil

from fbuild.flxbuild.process import Process

import config

class build_ocaml_exes(Process):
  def runme(self, EXES, OLIBRARIES, INCLUDES):
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
      modules = config.HOST_OCAML.compile_module([exe], **kwds)

      src = config.HOST_OCAML.link_exe(
        modules,
        os.path.splitext(exe)[0] + config.HOST_OCAML.options.EXT_EXE,
        libs=OLIBRARIES,
        **kwds)

      dst = os.path.join('bin', os.path.basename(src))

      if not self.quiet: print 'copying file', src, '->', dst
      shutil.copy(src, dst)
      output_exes.append(dst)

    return output_exes

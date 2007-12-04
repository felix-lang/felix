import os
import shutil

from fbuild.flxbuild.process import Process, processes
from fbuild.flxbuild.flxutil import unix2native

import config

class build_host_ocaml(Process):
  def copy_mli2ml(self, MLIS):
    for f in MLIS:
      f = unix2native(f)
      src = f+'.mli'
      dst = f+'.ml'
      if not self.quiet: print 'copying file', src, '->', dst
      shutil.copyfile(src, dst)


  def runme(self, pkg, pkgdict, *args):
    RAW_INTERFACES = pkgdict.get('caml_raw_interfaces', [])
    self.copy_mli2ml(RAW_INTERFACES)

    if config.HOST_OCAML.options.HAVE_OCAMLBUILD:
      return processes['build_ocamlbuild'].runme(pkg, pkgdict)

    LEXS = pkgdict.get("caml_lexes", [])
    PARSES = pkgdict.get("caml_parses", [])
    PGENPARSES = pkgdict.get("caml_pgenparses", [])
    DYPARSES = pkgdict.get("caml_dyparses", [])

    processes['build_ocaml_grammar'].runme(pkg, LEXS, PARSES, PGENPARSES, DYPARSES)

    INTERFACES = pkgdict.get("caml_interfaces", [])
    IMPLEMENTATIONS = pkgdict.get("caml_implementations", [])
    PACKS = pkgdict.get("caml_pack", [])
    EXES = pkgdict.get("caml_exes", [])
    INCLUDES = pkgdict.get("caml_include_paths", [])


    objects = processes['build_ocaml_modules'].runme(pkg, INTERFACES, IMPLEMENTATIONS, PACKS, INCLUDES)

    lib = pkgdict.get('caml_provide_lib', os.path.join('src', pkg + 'lib'))
    processes['build_ocaml_libraries'].runme(objects, lib)
    OLIBRARIES = pkgdict.get("caml_require_libs", [])

    processes['build_ocaml_exes'].runme(EXES, OLIBRARIES, INCLUDES)

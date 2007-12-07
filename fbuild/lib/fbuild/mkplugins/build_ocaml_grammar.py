from fbuild.flxbuild.process import Process

import config

class build_ocaml_grammar(Process):
  def runme(self, pkg, pkgdict, *args):
    LEXS = pkgdict.get("caml_lexes", [])
    PARSES = pkgdict.get("caml_parses", [])
    PGENPARSES = pkgdict.get("caml_pgenparses", [])
    DYPARSES = pkgdict.get("caml_dyparses", [])

    if not (LEXS or PARSES or PGENPARSES or DYPARSES):
      return

    print "CAML BUILDING GRAMMAR", pkg

    if LEXS:
      config.HOST_OCAML.gen_lexer(LEXS,
          outdir='build')

    if PARSES:
      config.HOST_OCAML.gen_parser(PARSES,
          outdir='build')

    if PGENPARSES:
      config.HOST_OCAML.gen_pgen_parser(PGENPARSES,
          outdir='build')

    if DYPARSES:
      config.HOST_OCAML.gen_dypgen_parser(DYPARSES,
          FLAGS=['--prio-pt', '--pv-obj', '--noemit-token-type'],
          outdir='build')

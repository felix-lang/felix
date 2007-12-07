import config
from fbuild.flxbuild.process import Process

class build_grammar(Process):
    def runme(self, pkg, pkgdict, *args):
        PGENPARSES = pkgdict.get("caml_pgenparses", [])
        DYPARSES = pkgdict.get("caml_dyparses", [])

        if not (PGENPARSES or DYPARSES):
            return

        print "CAML BUILDING DYPGEN GRAMMAR", pkg

        if PGENPARSES:
            config.HOST_OCAML.gen_pgen_parser(PGENPARSES,
                outdir='build',
            )

        if DYPARSES:
            config.HOST_OCAML.gen_dypgen_parser(DYPARSES,
                FLAGS=['--prio-pt', '--pv-obj', '--noemit-token-type'],
                outdir='build',
            )

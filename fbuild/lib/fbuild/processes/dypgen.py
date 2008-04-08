import os
import config

def build_grammar(pkg, pkgdict, *args):
    PGENPARSES = pkgdict.get("caml_pgenparses", [])
    DYPARSES = pkgdict.get("caml_dyparses", [])
    MODULES = pkgdict.get("caml_modules", [])

    for module in MODULES:
        dyp = config.HOST_OCAML.find_in_src_dir(module + '.dyp')
        if os.path.exists(dyp) or os.path.exists(os.path.join('build', dyp)):
            DYPARSES.append(module)

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

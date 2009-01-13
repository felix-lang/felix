import fbuild
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

def build(ocaml, ocamllex):
    path = Path('src', 'compiler', 'sex')
    dypgen = call('buildsystem.dyp.build_dypgen', ocaml, ocamllex)
    return ocaml.build_lib(path/'sex', Path.globall(
            path/'*.ml{,i}',
            dypgen(path/'sex_parse.dyp'),
            ocamllex(path/'sex_lex.mll')),
        libs=[
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.dyp.build_lib', ocaml)])

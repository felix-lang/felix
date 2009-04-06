import fbuild
from fbuild.functools import call
from fbuild.path import Path

# ------------------------------------------------------------------------------

def build(ocaml, ocamllex):
    path = Path('src', 'compiler', 'sex')
    dypgen = call('buildsystem.dypgen.build_exe', ocaml, ocamllex)
    return ocaml.build_lib(path/'sex', Path.globall(
            path/'*.ml{,i}',
            dypgen(path/'sex_parse.dyp'),
            ocamllex(path/'sex_lex.mll')),
        libs=[
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.dypgen.build_lib', ocaml)])

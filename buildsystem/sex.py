import fbuild
from fbuild.path import Path

# -----------------------------------------------------------------------------

def build(ocaml):
    path = Path('src', 'compiler', 'sex')
    dypgen = fbuild.env.run('buildsystem.dyp.build_dypgen', ocaml)
    return ocaml.builder.build_lib(path/'sex', [
            path/'*.ml{,i}',
            dypgen(path/'sex_parse.dyp'),
            ocaml.ocamllex(path/'sex_lex.mll'),
        ],
        libs=[
            fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
            fbuild.env.run('buildsystem.dyp.build_lib', ocaml),
        ])

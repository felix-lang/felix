import fbuild
import fbuild.packages.ocaml as ocaml
from fbuild.path import Path
from fbuild.record import Record

from buildsystem.dyp import Dypgen

# -----------------------------------------------------------------------------

def build_flx_misc(builder):
    path = Path('src/compiler/flx_misc')
    return ocaml.Library(path/'flx_misc',
        srcs=[path / '*.ml{,i}'],
        libs=['nums', 'str', 'unix'],
        builder=builder)

def build_flx_core(builder):
    path = Path('src/compiler/flx_core')
    return ocaml.Library(path / 'flx_core',
        srcs=[path / '*.ml{,i}'],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run('buildsystem.ocs.build', builder).lib,
        ],
        builder=builder)

def build_flx_version(builder):
    path = fbuild.buildroot / 'src/compiler/flx_version'
    return ocaml.Library(path / 'flx_version',
        srcs=[path / '*.ml{,i}'],
        builder=builder)

def build_flx_version_hook(builder):
    path = fbuild.buildroot / 'src/compiler/flx_version_hook'
    return ocaml.Library(path / 'flx_version_hook',
        srcs=[path / '*.ml{,i}'],
        libs=[fbuild.env.run(build_flx_version, builder)],
        builder=builder)

def build_flx_lex(builder):
    path = Path('src/compiler/flx_lex')
    return ocaml.Library(path/'flx_lex',
        srcs=[
            path / '*.ml{,i}',
            fbuild.buildroot/ path / '*.ml{,i}',
            ocaml.Ocamllex(fbuild.buildroot / path / 'flx_lex.mll'),
            Dypgen(fbuild.buildroot / path / 'flx_preparse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            fbuild.env.run('buildsystem.dyp.build_dyplib', builder),
            fbuild.env.run('buildsystem.ocs.build', builder).lib,
            fbuild.env.run('buildsystem.sex.build', builder),
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
        ],
        builder=builder)

def build_flx_parse(builder):
    path = Path('src/compiler/flx_parse')
    return ocaml.Library(path/'flx_parse',
        srcs=[
            path / '*.ml{,i}',
            Dypgen(fbuild.buildroot / path / 'flx_parse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            fbuild.env.run('buildsystem.dyp.build_dyplib', builder),
            fbuild.env.run('buildsystem.ocs.build', builder).lib,
            fbuild.env.run('buildsystem.sex.build', builder),
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_lex, builder),
        ],
        builder=builder)

def build_flxcclib(builder):
    path = fbuild.buildroot / 'src/compiler/flxcclib'
    return ocaml.Library(path / 'flxcclib',
        srcs=[path / '*.ml{,i}'],
        libs=[
            fbuild.env.run('buildsystem.cil.build', builder),
            fbuild.env.run(build_flx_core, builder),
        ],
        builder=builder)

def build_flx_desugar(builder):
    path = Path('src/compiler/flx_desugar')
    return ocaml.Library(path / 'flx_desugar',
        srcs=[path / '*.ml{,i}'],
        libs=[
            'nums',
            'unix',
            fbuild.env.run('buildsystem.dyp.build_dyplib', builder),
            fbuild.env.run('buildsystem.ocs.build', builder).lib,
            fbuild.env.run('buildsystem.sex.build', builder),
            fbuild.env.run('buildsystem.cil.build', builder),
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_lex, builder),
            fbuild.env.run(build_flx_parse, builder),
            fbuild.env.run(build_flxcclib, builder),
        ],
        builder=builder)

def build_inria_re(builder):
    path = Path('src/compiler/inria_re')
    return ocaml.Library(path / 'inria_re', [path / '*.ml{,i}'],
        builder=builder)

def build_flx_bind(builder):
    path = Path('src/compiler/flx_bind')
    return ocaml.Library(path / 'flx_bind', [path / '*.ml{,i}'],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_inria_re, builder),
        ],
        builder=builder)

def build_flx_frontend(builder):
    path = Path('src/compiler/flx_frontend')
    return ocaml.Library(path / 'flx_frontend', [path / '*.ml{,i}'],
        libs=[
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_bind, builder),
        ],
        builder=builder)

def build_flx_backend(builder):
    path = Path('src/compiler/flx_backend')
    return ocaml.Library(path / 'flx_backend', [
            path / '*.ml{,i}',
            fbuild.buildroot / path / '*.ml{,i}',
        ],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_bind, builder),
            fbuild.env.run(build_flxcclib, builder),
            fbuild.env.run(build_flx_frontend, builder),
        ],
        builder=builder)

def build_flx_drivers(builder):
    path = Path('src', 'compiler', 'drivers')

    lib = ocaml.Library(path / 'flx_driver', [
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}',
        ],
        libs=[
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flxcclib, builder),
        ],
        builder=builder)

    flxl = ocaml.Executable(fbuild.buildroot / 'bin/flxl', [path / 'flxl.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flx_lex, builder),
            fbuild.env.run(build_flx_desugar, builder),
        ],
        builder=builder)

    flxp = ocaml.Executable(fbuild.buildroot / 'bin/flxp', [path / 'flxp.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flx_lex, builder),
            fbuild.env.run(build_flx_parse, builder),
            fbuild.env.run(build_flx_desugar, builder),
        ],
        builder=builder)

    flxm = ocaml.Executable(fbuild.buildroot / 'bin/flxm', [path / 'flxm.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flx_parse, builder),
            fbuild.env.run(build_flx_desugar, builder),
        ],
        builder=builder)

    flxd = ocaml.Executable(fbuild.buildroot / 'bin/flxd', [path / 'flxd.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flx_desugar, builder),
        ],
        builder=builder)

    flxb = ocaml.Executable(fbuild.buildroot / 'bin/flxb', [path / 'flxb.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flx_parse, builder),
            fbuild.env.run(build_flx_desugar, builder),
            fbuild.env.run(build_flx_bind, builder),
            fbuild.env.run(build_flx_frontend, builder),
        ],
        builder=builder)

    flxg = ocaml.Executable(fbuild.buildroot / 'bin/flxg', [path / 'flxg.ml'],
        libs=[
            lib,
            fbuild.env.run(build_flx_misc, builder),
            fbuild.env.run(build_flx_core, builder),
            fbuild.env.run(build_flx_version, builder),
            fbuild.env.run(build_flx_version_hook, builder),
            fbuild.env.run(build_flxcclib, builder),
            fbuild.env.run(build_flx_desugar, builder),
            fbuild.env.run(build_flx_bind, builder),
            fbuild.env.run(build_flx_frontend, builder),
            fbuild.env.run(build_flx_backend, builder),
        ],
        builder=builder)

    return Record(
        flxl=flxl,
        flxp=flxp,
        flxm=flxm,
        flxd=flxd,
        flxb=flxb,
        flxg=flxg,
    )

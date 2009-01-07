import fbuild
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_misc(ocaml):
    path = Path('src/compiler/flx_misc')
    return ocaml.builder.build_lib(path / 'flx_misc',
        srcs=[path / '*.ml{,i}'],
        libs=['nums', 'str', 'unix'])

def build_flx_core(ocaml):
    path = Path('src/compiler/flx_core')
    return ocaml.builder.build_lib(path / 'flx_core',
        srcs=[path / '*.ml{,i}'],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
        ])

def build_flx_version(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version'
    return ocaml.builder.build_lib(path / 'flx_version',
        srcs=[path / '*.ml{,i}'])

def build_flx_version_hook(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version_hook'
    return ocaml.builder.build_lib(path / 'flx_version_hook',
        srcs=[path / '*.ml{,i}'],
        libs=[fbuild.env.run(build_flx_version, ocaml)])

def build_flx_lex(ocaml):
    path = Path('src/compiler/flx_lex')
    dypgen = fbuild.env.run('buildsystem.dyp.build_dypgen', ocaml)
    return ocaml.builder.build_lib(path/'flx_lex',
        srcs=[
            path / '*.ml{,i}',
            fbuild.buildroot/ path / '*.ml{,i}',
            ocaml.ocamllex(fbuild.buildroot / path / 'flx_lex.mll'),
            dypgen(fbuild.buildroot / path / 'flx_preparse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            fbuild.env.run('buildsystem.dyp.build_lib', ocaml),
            fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
            fbuild.env.run('buildsystem.sex.build', ocaml),
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flx_version, ocaml),
        ])

def build_flx_parse(ocaml):
    path = Path('src/compiler/flx_parse')
    dypgen = fbuild.env.run('buildsystem.dyp.build_dypgen', ocaml)
    return ocaml.builder.build_lib(path/'flx_parse',
        srcs=[
            path / '*.ml{,i}',
            dypgen(fbuild.buildroot / path / 'flx_parse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type']),
        ],
        libs=[
            fbuild.env.run('buildsystem.dyp.build_lib', ocaml),
            fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
            fbuild.env.run('buildsystem.sex.build', ocaml),
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flx_version, ocaml),
            fbuild.env.run(build_flx_lex, ocaml),
        ])

def build_flxcclib(ocaml):
    path = fbuild.buildroot / 'src/compiler/flxcclib'
    return ocaml.builder.build_lib(path / 'flxcclib', [path / '*.ml{,i}'],
        includes=[fbuild.buildroot / 'src/compiler/cil/src'],
        libs=[
            fbuild.env.run('buildsystem.cil.build', ocaml),
            fbuild.env.run(build_flx_core, ocaml),
        ])

def build_flx_desugar(ocaml):
    path = Path('src/compiler/flx_desugar')

    return ocaml.builder.build_lib(path / 'flx_desugar', [path / '*.ml{,i}'],
        includes=[
            fbuild.buildroot / 'src/compiler/cil/ocamlutil',
            fbuild.buildroot / 'src/compiler/cil/src',
            fbuild.buildroot / 'src/compiler/cil/src/frontc',
        ],
        libs=[
            'nums',
            'unix',
            fbuild.env.run('buildsystem.dyp.build_lib', ocaml),
            fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
            fbuild.env.run('buildsystem.sex.build', ocaml),
            fbuild.env.run('buildsystem.cil.build', ocaml),
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flx_version, ocaml),
            fbuild.env.run(build_flx_lex, ocaml),
            fbuild.env.run(build_flx_parse, ocaml),
            fbuild.env.run(build_flxcclib, ocaml),
        ])

def build_inria_re(ocaml):
    path = Path('src/compiler/inria_re')
    return ocaml.builder.build_lib(path / 'inria_re', [path / '*.ml{,i}'])

def build_flx_bind(ocaml):
    path = Path('src/compiler/flx_bind')
    return ocaml.builder.build_lib(path / 'flx_bind', [path / '*.ml{,i}'],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_inria_re, ocaml),
        ])

def build_flx_frontend(ocaml):
    path = Path('src/compiler/flx_frontend')
    return ocaml.builder.build_lib(path / 'flx_frontend', [path / '*.ml{,i}'],
        libs=[
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flx_bind, ocaml),
        ])

def build_flx_backend(ocaml):
    path = Path('src/compiler/flx_backend')
    return ocaml.builder.build_lib(path / 'flx_backend', [
            path / '*.ml{,i}',
            fbuild.buildroot / path / '*.ml{,i}',
        ],
        libs=[
            'nums',
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flx_bind, ocaml),
            fbuild.env.run(build_flxcclib, ocaml),
            fbuild.env.run(build_flx_frontend, ocaml),
        ])

def build_flx_drivers(ocaml):
    path = Path('src', 'compiler', 'drivers')

    lib = ocaml.builder.build_lib(path / 'flx_driver', [
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}',
        ],
        libs=[
            fbuild.env.run(build_flx_misc, ocaml),
            fbuild.env.run(build_flx_core, ocaml),
            fbuild.env.run(build_flxcclib, ocaml),
        ])

    libs = [
        'nums', 'unix', 'str',
        fbuild.env.run('buildsystem.ocs.build_lib', ocaml),
        fbuild.env.run('buildsystem.sex.build', ocaml),
        fbuild.env.run('buildsystem.dyp.build_lib', ocaml),
        fbuild.env.run('buildsystem.cil.build', ocaml),
        fbuild.env.run(build_flx_misc, ocaml),
        fbuild.env.run(build_flx_core, ocaml),
        fbuild.env.run(build_flx_version, ocaml),
        fbuild.env.run(build_flx_version_hook, ocaml),
        fbuild.env.run(build_flx_lex, ocaml),
        fbuild.env.run(build_flx_parse, ocaml),
        fbuild.env.run(build_flxcclib, ocaml),
        fbuild.env.run(build_flx_desugar, ocaml),
        fbuild.env.run(build_inria_re, ocaml),
        fbuild.env.run(build_flx_bind, ocaml),
        fbuild.env.run(build_flx_frontend, ocaml),
        fbuild.env.run(build_flx_backend, ocaml),
        lib,
    ]

    flxl = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxl',
        [path / 'flxl.ml'], libs=libs)

    flxp = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxp',
        [path / 'flxp.ml'], libs=libs)

    flxm = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxm',
        [path / 'flxm.ml'], libs=libs)

    flxd = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxd',
        [path / 'flxd.ml'], libs=libs)

    flxb = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxb',
        [path / 'flxb.ml'], libs=libs)

    flxg = ocaml.builder.build_exe(fbuild.buildroot / 'bin/flxg',
        [path / 'flxg.ml'], libs=libs)

    return Record(
        flxl=flxl,
        flxp=flxp,
        flxm=flxm,
        flxd=flxd,
        flxb=flxb,
        flxg=flxg,
    )

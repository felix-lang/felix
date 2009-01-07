import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_misc(ocaml):
    path = Path('src/compiler/flx_misc')
    return ocaml.ocaml.build_lib(path / 'flx_misc',
        srcs=Path.glob(path / '*.ml{,i}'),
        external_libs=['nums', 'str', 'unix'])

def build_flx_core(ocaml):
    path = Path('src/compiler/flx_core')
    return ocaml.ocaml.build_lib(path / 'flx_core',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call(build_flx_misc, ocaml),
            call('buildsystem.ocs.build_lib', ocaml)],
        external_libs=['nums'])

def build_flx_version(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version'
    return ocaml.ocaml.build_lib(path / 'flx_version',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_version_hook(ocaml):
    path = fbuild.buildroot / 'src/compiler/flx_version_hook'
    return ocaml.ocaml.build_lib(path / 'flx_version_hook',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[call(build_flx_version, ocaml)])

def build_flx_lex(ocaml):
    path = Path('src/compiler/flx_lex')
    dypgen = call('buildsystem.dyp.build_dypgen', ocaml)
    return ocaml.ocaml.build_lib(path/'flx_lex',
        srcs=Path.globall(
            path / '*.ml{,i}',
            fbuild.buildroot/ path / '*.ml{,i}',
            ocaml.ocamllex(fbuild.buildroot / path / 'flx_lex.mll'),
            dypgen(fbuild.buildroot / path / 'flx_preparse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dyp.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml),
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flx_version, ocaml)])

def build_flx_parse(ocaml):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dyp.build_dypgen', ocaml)
    return ocaml.ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(fbuild.buildroot / path / 'flx_parse.dyp',
                flags=['--prio-pt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dyp.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml),
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flx_version, ocaml),
            call(build_flx_lex, ocaml)])

def build_flxcclib(ocaml):
    path = fbuild.buildroot / 'src/compiler/flxcclib'
    return ocaml.ocaml.build_lib(path / 'flxcclib',
        srcs=Path.glob(path / '*.ml{,i}'),
        includes=[fbuild.buildroot / 'src/compiler/cil/src'],
        libs=[
            call('buildsystem.cil.build', ocaml),
            call(build_flx_core, ocaml)])

def build_flx_desugar(ocaml):
    path = Path('src/compiler/flx_desugar')

    return ocaml.ocaml.build_lib(path / 'flx_desugar',
        srcs=Path.glob(path / '*.ml{,i}'),
        includes=[
            fbuild.buildroot / 'src/compiler/cil/ocamlutil',
            fbuild.buildroot / 'src/compiler/cil/src',
            fbuild.buildroot / 'src/compiler/cil/src/frontc'],
        libs=[
            call('buildsystem.dyp.build_lib', ocaml),
            call('buildsystem.ocs.build_lib', ocaml),
            call('buildsystem.sex.build', ocaml),
            call('buildsystem.cil.build', ocaml),
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flx_version, ocaml),
            call(build_flx_lex, ocaml),
            call(build_flx_parse, ocaml),
            call(build_flxcclib, ocaml)],
        external_libs=['nums', 'unix'])

def build_inria_re(ocaml):
    path = Path('src/compiler/inria_re')
    return ocaml.ocaml.build_lib(path / 'inria_re',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_bind(ocaml):
    path = Path('src/compiler/flx_bind')
    return ocaml.ocaml.build_lib(path / 'flx_bind',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_inria_re, ocaml)],
        external_libs=['nums'])

def build_flx_frontend(ocaml):
    path = Path('src/compiler/flx_frontend')
    return ocaml.ocaml.build_lib(path / 'flx_frontend',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flx_bind, ocaml)])

def build_flx_backend(ocaml):
    path = Path('src/compiler/flx_backend')
    return ocaml.ocaml.build_lib(path / 'flx_backend',
        srcs=Path.globall(
            path / '*.ml{,i}',
            fbuild.buildroot / path / '*.ml{,i}'),
        libs=[
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flx_bind, ocaml),
            call(build_flxcclib, ocaml),
            call(build_flx_frontend, ocaml)],
        external_libs=['nums'])

def build_flx_drivers(ocaml):
    path = Path('src', 'compiler', 'drivers')

    lib = ocaml.ocaml.build_lib(path / 'flx_driver',
        Path.globall(
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}'),
        libs=[
            call(build_flx_misc, ocaml),
            call(build_flx_core, ocaml),
            call(build_flxcclib, ocaml)])

    libs = [
        call('buildsystem.ocs.build_lib', ocaml),
        call('buildsystem.sex.build', ocaml),
        call('buildsystem.dyp.build_lib', ocaml),
        call('buildsystem.cil.build', ocaml),
        call(build_flx_misc, ocaml),
        call(build_flx_core, ocaml),
        call(build_flx_version, ocaml),
        call(build_flx_version_hook, ocaml),
        call(build_flx_lex, ocaml),
        call(build_flx_parse, ocaml),
        call(build_flxcclib, ocaml),
        call(build_flx_desugar, ocaml),
        call(build_inria_re, ocaml),
        call(build_flx_bind, ocaml),
        call(build_flx_frontend, ocaml),
        call(build_flx_backend, ocaml),
        lib]

    external_libs = ['nums', 'unix', 'str']

    flxl = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxl',
        [path / 'flxl.ml'], libs=libs, external_libs=external_libs)

    flxp = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxp',
        [path / 'flxp.ml'], libs=libs, external_libs=external_libs)

    flxm = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxm',
        [path / 'flxm.ml'], libs=libs, external_libs=external_libs)

    flxd = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxd',
        [path / 'flxd.ml'], libs=libs, external_libs=external_libs)

    flxb = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxb',
        [path / 'flxb.ml'], libs=libs, external_libs=external_libs)

    flxg = ocaml.ocaml.build_exe(fbuild.buildroot / 'bin/flxg',
        [path / 'flxg.ml'], libs=libs, external_libs=external_libs)

    return Record(
        flxl=flxl,
        flxp=flxp,
        flxm=flxm,
        flxd=flxd,
        flxb=flxb,
        flxg=flxg,
    )

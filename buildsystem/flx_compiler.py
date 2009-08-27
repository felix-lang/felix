import fbuild
import fbuild.builders.llvm
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

# ------------------------------------------------------------------------------

def build_flx_misc(phase):
    path = Path('src/compiler/flx_misc')
    return phase.ocaml.build_lib(path / 'flx_misc',
        srcs=Path.glob(path / '*.ml{,i}'),
        external_libs=['nums', 'str', 'unix'])

def build_flx_core(phase):
    path = Path('src/compiler/flx_core')
    return phase.ocaml.build_lib(path / 'flx_core',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            call('buildsystem.ocs.build_lib', phase)],
        external_libs=['nums'])

def build_flx_version(phase):
    path = phase.ctx.buildroot / 'src/compiler/flx_version'
    return phase.ocaml.build_lib(path / 'flx_version',
        srcs=Path.glob(path / '*.ml{,i}'))

def build_flx_version_hook(phase):
    path = phase.ctx.buildroot / 'src/compiler/flx_version_hook'
    return phase.ocaml.build_lib(path / 'flx_version_hook',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[build_flx_version(phase)])

def build_flx_lex(phase):
    path = Path('src/compiler/flx_lex')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_lex',
        srcs=Path.globall(
            path / '*.ml{,i}'
            ),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_version(phase)])

def build_flx_parse(phase):
    path = Path('src/compiler/flx_parse')
    dypgen = call('buildsystem.dypgen.build_exe', phase)
    return phase.ocaml.build_lib(path/'flx_parse',
        srcs=Path.globall(
            path / '*.ml{,i}',
            dypgen(path / 'flx_parse.dyp',
                flags=['--no-undef-nt', '--pv-obj', '--noemit-token-type'])),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_version(phase),
            build_flx_lex(phase)])

def build_flx_desugar(phase):
    path = Path('src/compiler/flx_desugar')

    return phase.ocaml.build_lib(path / 'flx_desugar',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            call('buildsystem.dypgen.build_lib', phase),
            call('buildsystem.ocs.build_lib', phase),
            call('buildsystem.sex.build', phase),
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_version(phase),
            build_flx_lex(phase),
            build_flx_parse(phase)],
        external_libs=['nums', 'unix'])

def build_flx_bind(phase):
    path = Path('src/compiler/flx_bind')
    return phase.ocaml.build_lib(path / 'flx_bind',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_desugar(phase)],
        external_libs=['nums'])

def build_flx_frontend(phase):
    path = Path('src/compiler/flx_frontend')
    return phase.ocaml.build_lib(path / 'flx_frontend',
        srcs=Path.glob(path / '*.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_desugar(phase),
            build_flx_bind(phase)])

def build_flx_cpp_backend(phase):
    path = Path('src/compiler/flx_cpp_backend')
    return phase.ocaml.build_lib(path / 'flx_cpp_backend',
        srcs=Path.globall(
            path / '*.ml{,i}',
            phase.ctx.buildroot / path / '*.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_desugar(phase),
            build_flx_bind(phase),
            build_flx_frontend(phase)],
        external_libs=['nums'])

def build_flx_llvm_backend(phase):
    path = Path('src/compiler/flx_llvm_backend')
    return phase.ocaml.build_lib(path / 'flx_llvm_backend',
        srcs=Path.globall(path / '*.ml{,i}'),
        includes=['/tmp/llvm/lib/ocaml'],
        libs=[build_flx_core(phase)])

def build_flx_drivers(ctx, phase):
    path = Path('src', 'compiler', 'drivers')

    lib = phase.ocaml.build_lib(path / 'flx_driver',
        Path.globall(
            path / 'flx_terminate.ml{,i}',
            path / 'flx_flxopt.ml{,i}'),
        libs=[
            build_flx_misc(phase),
            build_flx_core(phase),
            build_flx_cpp_backend(phase)])

    libs = [
        call('buildsystem.ocs.build_lib', phase),
        call('buildsystem.sex.build', phase),
        call('buildsystem.dypgen.build_lib', phase),
        build_flx_misc(phase),
        build_flx_core(phase),
        build_flx_version(phase),
        build_flx_version_hook(phase),
        build_flx_lex(phase),
        build_flx_parse(phase),
        build_flx_desugar(phase),
        build_flx_bind(phase),
        build_flx_frontend(phase),
        build_flx_cpp_backend(phase)]

    external_libs = ['nums', 'unix', 'str']

    flxp = phase.ocaml.build_exe('bin/flxp',
        [path / 'flxp.ml'], libs=libs + [lib], external_libs=external_libs)

    flxm = phase.ocaml.build_exe('bin/flxm',
        [path / 'flxm.ml'], libs=libs + [lib], external_libs=external_libs)

    flxd = phase.ocaml.build_exe('bin/flxd',
        [path / 'flxd.ml'], libs=libs + [lib], external_libs=external_libs)

    flxb = phase.ocaml.build_exe('bin/flxb',
        [path / 'flxb.ml'], libs=libs + [lib], external_libs=external_libs)

    flxg = phase.ocaml.build_exe('bin/flxg',
        [path / 'flxg.ml'], libs=libs + [lib], external_libs=external_libs)

    # Don't compile flxc if llvm isn't installed
    try:
        llvm_config = fbuild.builders.llvm.LlvmConfig(ctx,
            requires_version=(2, '6svn'))
    except fbuild.ConfigFailed as err:
        ctx.logger.failed(err)
        flxc = None
    else:
        flxc = phase.ocaml.build_exe('bin/flxc',
            Path('src/compiler/flxc/*.ml{,i}').glob(),
            includes=['/tmp/llvm/lib/ocaml'],
            libs=libs + [build_flx_llvm_backend(phase)],
            external_libs=external_libs + ['llvm', 'llvm_analysis'],
            cc=phase.cxx.static.compiler.gcc.exe)

    return Record(
        flxp=flxp,
        flxm=flxm,
        flxd=flxd,
        flxb=flxb,
        flxg=flxg,
        flxc=flxc,
    )

import fbuild
import fbuild.builders.file
import os
# ------------------------------------------------------------------------------

def build(phase, felix):
    try:
        exe = felix.compile(phase.ctx.buildroot/'tools/flx_ls.flx', static=True)
        fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    except:
        print("Warning : flx_ls not built. Continuing..." )

    try:
        exe = felix.compile(phase.ctx.buildroot/'tools/flx_cp.flx', static=True)
        fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    except:
        print("Warning : flx_cp not built. Continuing..." )

    try:
        exe = felix.compile(phase.ctx.buildroot/'tools/webserver.flx', static=True)
        fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    except:
        print("Warning : webserver not built. Continuing..." )

    try:
        exe = felix.compile(phase.ctx.buildroot/'wiki/wiki.flx', static=True)
        fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    except:
        print("Warning : wiki not built. Continuing..." )

    try:
      os.mkdir(phase.ctx.buildroot/'shlib')
    except:
      pass

    shlib = felix.compile(phase.ctx.buildroot/'tools/ocaml2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    shlib = felix.compile(phase.ctx.buildroot/'tools/py2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    shlib = felix.compile(phase.ctx.buildroot/'tools/fdoc2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    shlib = felix.compile(phase.ctx.buildroot/'tools/flx2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    shlib = felix.compile(phase.ctx.buildroot/'tools/cpp2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    shlib = felix.compile(phase.ctx.buildroot/'tools/fpc2html.flx')
    fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')

    exe = felix.compile(phase.ctx.buildroot/'tools/norK.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile(phase.ctx.buildroot/'tools/rentut.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile(phase.ctx.buildroot/'tools/mktutindex.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile(phase.ctx.buildroot/'tools/flx_perror.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')





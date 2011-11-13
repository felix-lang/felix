import fbuild
import fbuild.builders.file

# ------------------------------------------------------------------------------

def build(phase, felix):
    exe = felix.compile(phase.ctx.buildroot/'tools/flx_ls.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile(phase.ctx.buildroot/'tools/flx_cp.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile(phase.ctx.buildroot/'tools/webserver.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

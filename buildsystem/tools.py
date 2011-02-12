import fbuild
import fbuild.builders.file

# ------------------------------------------------------------------------------

def build(phase, felix):
    exe = felix.compile('tools/flx_ls.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile('tools/flx_cp.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile('tools/webserver.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

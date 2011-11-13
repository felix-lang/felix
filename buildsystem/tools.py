import fbuild
import fbuild.builders.file

# ------------------------------------------------------------------------------

def build(phase, felix):
    exe = felix.compile('src/tools/flx_ls.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile('src/tools/flx_cp.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    exe = felix.compile('src/tools/webserver.flx', static=True)
    fbuild.builders.file.copy(phase.ctx, exe, 'bin')

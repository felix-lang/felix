import fbuild.builders.sdl
import fbuild.builders.text
import fbuild.config.c.sdl
import fbuild.config.c.mpi
import fbuild.config.c.pari
from fbuild.path import Path

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    dsts = []

    dsts.extend(buildsystem.copy_to(phase.ctx,Path (phase.ctx.buildroot)/"lib/gnu/gmp",
            Path('src/lib/gnu/gmp/*.flx').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,
        phase.ctx.buildroot / 'lib/GL', Path('src/opengl/*.flx').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,
        phase.ctx.buildroot / 'lib/GL', Path('src/glut/*.flx').glob()))

    if phase.sdl_config:
        sdl_fpc = fbuild.builders.text.autoconf_config_file(phase.ctx,
            'src/sdl/sdl.fpc', 'src/sdl/sdl.fpc.in', {
                'VERSION': phase.sdl_config.version(),
                'CFLAGS': phase.sdl_config.cflags(),
                'SHARED_LIBS': phase.sdl_config.libs(),
                'STATIC_LIBS': phase.sdl_config.static_libs(),
            })

        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx, [sdl_fpc]))

    dsts.extend(buildsystem.copy_to(phase.ctx,
            phase.ctx.buildroot / 'lib/SDL', Path('src/sdl/*.flx').glob()))

    return dsts

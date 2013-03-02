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

    return dsts

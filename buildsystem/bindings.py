import fbuild.builders.sdl
import fbuild.builders.text
import fbuild.config.c.gsl
import fbuild.config.c.sdl
import fbuild.config.c.mpi
import fbuild.config.c.pari
from fbuild.path import Path

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    dsts = []

    if fbuild.config.c.gsl.gsl_gsl_blas_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/lib/gnu/gsl/*.fpc').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,Path (phase.ctx.buildroot)/"lib/gnu/gsl",
            Path('src/lib/gnu/gsl/*.flx').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,Path (phase.ctx.buildroot)/"lib/gnu/gmp",
            Path('src/lib/gnu/gmp/*.flx').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,
        phase.ctx.buildroot / 'lib/GL', Path('src/opengl/*.flx').glob()))

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

    if fbuild.config.c.mpi.mpi_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/mpi/*.fpc').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,
            Path(phase.ctx.buildroot) / 'lib/mpi', Path('src/mpi/*.flx').glob()))

    if fbuild.config.c.pari.pari_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/pari/*.fpc').glob()))

    dsts.extend(buildsystem.copy_to(phase.ctx,
            Path(phase.ctx.buildroot) / "lib/pari",
            Path('src/pari/*.flx').glob()))

    return dsts

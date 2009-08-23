import fbuild.config.c.gsl
import fbuild.config.c.mpi
import fbuild.config.c.pari
from fbuild.path import Path
import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    dsts = []

    if fbuild.config.c.gsl.gsl_gsl_blas_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/gsl/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/gsl/*.flx').glob()))

    if fbuild.config.c.mpi.mpi_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/mpi/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/mpi/*.flx').glob()))

    if fbuild.config.c.pari.pari_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(phase.ctx,
            Path('src/pari/*.fpc').glob()))

        dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
            Path('src/pari/*.flx').glob()))

    return dsts

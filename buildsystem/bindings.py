import fbuild.config.c.gsl
from fbuild.path import Path
import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    dsts = []

    if fbuild.config.c.gsl.gsl_gsl_blas_h(phase.cxx.shared).header:
        dsts.extend(buildsystem.copy_fpc_to_config(
            Path('src/gsl/*.fpc').glob()))
        dsts.extend(buildsystem.copy_flxs_to_lib(Path('src/gsl/*.flx').glob()))

    return dsts

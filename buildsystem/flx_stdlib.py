from fbuild.path import Path

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(phase):
    path = Path(phase.ctx.buildroot/'share'/'src/lib')

    dsts = []
    dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
        (path / '*.flx{,h}').glob()))
    dsts.extend(buildsystem.copy_flxs_to_lib(phase.ctx,
        (path / '*.fdoc').glob()))

    dsts.extend(buildsystem.copy_flxs_to_libstd(phase.ctx,
        (path / 'std/*.flx{,h}').glob()))
    dsts.extend(buildsystem.copy_flxs_to_libstd(phase.ctx,
        (path / 'std/*.fdoc').glob()))

    dsts.extend(buildsystem.copy_flxs_to_libstd_posix(phase.ctx,
        (path / 'std/posix/*.flx{,h}').glob()))
    dsts.extend(buildsystem.copy_flxs_to_libstd_posix(phase.ctx,
        (path / 'std/posix/*.fdoc').glob()))

    dsts.extend(buildsystem.copy_flxs_to_libstl(phase.ctx,
        (path / 'stl/*.flx{,h}').glob()))
    dsts.extend(buildsystem.copy_flxs_to_libstl(phase.ctx,
        (path / 'stl/*.fdoc').glob()))

    return dsts

from fbuild.path import Path

import buildsystem

# ------------------------------------------------------------------------------

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path('src/lib/*.flx{,h}').glob())

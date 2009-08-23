import os

import buildsystem
from fbuild.path import Path

# ------------------------------------------------------------------------------

def copy_user_fpcs(ctx):
    home = os.getenv("HOME")
    if home is not None:
        return buildsystem.copy_fpc_to_config(ctx,
            Path(home, ".felix", "config", "*.fpc").glob())

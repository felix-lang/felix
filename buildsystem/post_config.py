import buildsystem
import os
import glob
from fbuild.path import Path

def copy_user_fpcs():
  try:
    home = os.getenv("HOME")
    globspec = Path(home, ".felix", "config", "*.fpc")
    files = globspec.glob()
    buildsystem.copy_fpc_to_config(files)
  except:
    print("copying files from $HOME directory failed, continuing anyhow")
    pass


import os
import sys

from fbuild.flxbuild.flxutil import xqt

class compiler_base(object):
  def __init__(self, verbose=False, quiet=False):
    class Options:
      pass
    self.options = Options()
    self.verbose = verbose
    self.quiet = quiet

  def shell(self, *args, **kwds):
    kwds.setdefault('verbose', self.verbose)
    kwds.setdefault('quiet', self.quiet)

    return xqt(*args, **kwds)

  def set_options(self):
    pass

  def save_options(self, filename):
    f = open(filename, "w")
    ks = self.options.__dict__.keys()
    ks.sort()
    for k in ks:
      if k[0] != '_': # do not save __builtins__
        v = self.options.__dict__[k]
        f.write(k+'='+repr(v) + "\n")
    f.close()


  def load_options(self,filename):
    f = open(filename)
    exec f in self.options.__dict__
    f.close()

  def find_in_src_dir(self, filename):
    # check first if the file exists in the felix directory
    # if it does, use it instead of the one in the local directory
    try:
      from config import src_dir
    except ImportError:
      pass
    else:
      f = os.path.join(src_dir, filename)
      if os.path.exists(f):
        return f

    return filename

import os
import glob
import time

# forward import of set
try:
  set
except NameError:
  from sets import Set as set

from fbuild.flxbuild.flxutil import xqt, newest_filetime, filetime, fmtime, mkdirs
from fbuild.path import glob_paths
import config

# global process registry
processes = {}

class Process(object):
  help = ''

  def __init__(self,
      verbose=False,
      quiet=False,
      optimise=False,
      debug=False,
      force=False,
      options=[]):
    self.failures = []
    self.successes = []
    self.dummy = 0
    self.used = False
    self.verbose = verbose
    self.quiet = quiet
    self.optimise = optimise
    self.debug = debug
    self.force = force
    self.options = options

  def __call__(self, *args, **kwds):
    return self.runme(*args, **kwds)

  def shell(self, *args, **kwds):
    kwds.setdefault('verbose', self.verbose)
    kwds.setdefault('quiet', self.quiet)

    return xqt(*args, **kwds)

  def __str__(self):
    return self.__class__.__name


def get_latest_src_time(pkgdict):
  filenames = set()

  # check out iscr files
  for iscr in pkgdict.get("iscr_source",[]):
    base = os.path.basename(os.path.splitext(iscr)[0])
    f = open(os.path.join('manifests', base + '.deps'))
    try:
      filenames.update([line.strip() for line in f])
    finally:
      f.close()

  # check out xfiles
  root = pkgdict.get('root', config.FLX_DIR)
  xfiles = pkgdict.get('xfiles', [])

  filenames.update(glob_paths(xfiles, root))

  return newest_filetime(filenames)


def enstamp(stamp, quiet):
  mkdirs(os.path.split(stamp)[0])
  f = open(stamp,"w")
  try:
    print >> f, fmtime(time.time())
  finally:
    f.close()

  if not quiet:
    print 'Writing Stamp File:', stamp

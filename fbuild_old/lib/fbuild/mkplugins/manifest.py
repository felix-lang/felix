import os
import glob
import re

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import filetime, newest_filetime, mkdirs

import config

class manifest(Process):
  def make_manifest(self, pkg, deps_filename):
    regex = re.compile(r"CREATING .* NAMED FILE SOURCE (.*) \[")

    mkdirs("manifests")
    f = open(deps_filename, "w")
    try:
      for line in self.shell(config.ISCR, '--trace=sources', '--trace=changes', pkg):
        m = regex.match(line)
        if m:
          dep = m.group(1)
          print "Source File:", dep
          print >> f, os.path.join(config.FLX_LPARCHIVE, 'lpsrc', dep)
    finally:
      f.close()

  def preprocess(self):
    paks = glob.glob(os.path.join(config.FLX_LPARCHIVE, "lpsrc", "*.pak"))
    for pak in paks:
      base = os.path.basename(os.path.splitext(pak)[0])
      deps_filename = os.path.join("manifests", base + '.deps')

      if not os.path.exists(deps_filename):
        print "New pak", pak, "BUILDING MANIFEST"
        self.make_manifest(pak, deps_filename)
      else:
        manifest_time = filetime(deps_filename)

        f = open(deps_filename)
        try:
          src_time = newest_filetime([line.strip() for line in f] + [pak])
          if src_time > manifest_time:
            print "Changed pak", pak, "REBUILDING MANIFEST"
            self.make_manifest(pak, deps_filename)
        finally:
          f.close()

import os
import glob

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import erasefile

import config

# cleans products, but not extracted sources
class clean_run(Process):
  help = 'remove generated C++ and binaries from test locations'

  def runme(self, *args):
    for d in glob.glob(os.path.join("pkg-stamps", "*.run")):
      print "Removing " + d
      erasefile(d)
    for d in glob.glob(os.path.join("pkg-stamps", "*.test")):
      print "Removing " + d
      erasefile(d)
    for d in glob.glob(os.path.join("pkg-stamps", "*","*.test")):
      print "Removing " + d
      erasefile(d)
    for d in glob.glob(os.path.join("pkg-stamps", "*","*","*.test")):
      print "Removing " + d
      erasefile(d)
    for d in glob.glob(os.path.join("pkg-stamps", "*","*","*","*.test")):
      print "Removing " + d
      erasefile(d)
    exit(0)

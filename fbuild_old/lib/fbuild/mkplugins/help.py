import sys

from fbuild.flxbuild.process import Process, processes
from fbuild.flxbuild.package import pkgreqs, pkgdeps

class help(Process):
  help = 'print this help message'

  def preprocess(self):
    # PRINT PACKAGE DEPENDENCY INFORMATION
    print "REQS: "
    ks=pkgreqs.keys()
    ks.sort()
    length = 0
    for k in ks:
      length = max(len(k), length)
    for k in ks:
      print "  ", k.ljust(length), pkgreqs[k]

    print
    print "DEPS: "
    ks = pkgdeps.keys()
    ks.sort()
    length = 0
    for k in ks:
      length = max(len(k), length)
    for k in ks:
      print "  ", k.ljust(length), pkgdeps[k]

    items = []
    max_len = 0
    for name, process in processes.items():
      if process.help:
        max_len = max(max_len, len(name))
        items.append((name, process.help))

    items.sort()

    for name, help in items:
      print '%s %s' % (name.ljust(max_len), help)

    sys.exit(0)

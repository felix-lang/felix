from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import Tee

class run_known_failed_tests(Process):
  help = 'mark tests failed that are known to fail without running'

  def __init__(self, *args, **kwds):
    super(run_known_failed_tests, self).__init__(*args, **kwds)
    self.dummy = 1

  def runme(self, pkg, pkgdict,pkgsummary):
    bad_tests = pkgdict.get("known_failed_tests",[])
    bad_tests.sort()

    for testfile in bad_tests:
      log = Tee()
      log.write("**** KNOWN FAILED TESTING PACKAGE %s : %s ****\n" % (pkg, testfile))

      # don't even bother to compile
      self.failures.append((pkg, testfile, ""))

    return 1 # don't report any failure!

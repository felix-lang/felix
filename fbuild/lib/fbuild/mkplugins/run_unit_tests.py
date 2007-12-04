from fbuild.flxbuild.testprocess import TestProcess

class run_unit_tests(TestProcess):
  help = 'run unit tests'

  def runme(self, pkg, pkgdict, pkgsummary):
    unit_tests = pkgdict.get("unit_tests",[])
    unit_tests.sort()

    return self.run_tests(pkg, pkgdict, pkgsummary, unit_tests, 1, 1, 1,
      "*** UNIT TESTING %s : %%s ****\n" % pkg)

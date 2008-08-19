from fbuild.flxbuild.testprocess import TestProcess

class run_dynamic_unit_tests(TestProcess):
  help = 'run dynamic unit tests'

  def runme(self, pkg, pkgdict, pkgsummary):
    dynamic_unit_tests = pkgdict.get("dynamic_unit_tests",[])
    dynamic_unit_tests.sort()

    return self.run_tests(pkg, pkgdict, pkgsummary, dynamic_unit_tests, 1, 0, 1,
      "*** DYNAMIC UNIT TESTING %s : %%s ****\n" % pkg)

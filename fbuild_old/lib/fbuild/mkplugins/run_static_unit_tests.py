from fbuild.flxbuild.testprocess import TestProcess

class run_static_unit_tests(TestProcess):
  help = 'run static unit tests'

  def runme(self, pkg, pkgdict, pkgsummary):
    static_unit_tests = pkgdict.get("static_unit_tests",[])
    static_unit_tests.sort()

    return self.run_tests(pkg, pkgdict, pkgsummary, static_unit_tests, 1, 1, 0,
      "*** STATIC UNIT TESTING %s : %%s ****\n" % pkg)

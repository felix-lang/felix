from fbuild.flxbuild.testprocess import TestProcess

# these tests are units tests with non-deterministic results
class run_completion_tests(TestProcess):
  help = 'run tests that just need to finish'

  def runme(self, pkg, pkgdict, pkgsummary):
    completion_tests = pkgdict.get("completion_tests",[])
    completion_tests.sort()

    return self.run_tests(pkg, pkgdict, pkgsummary, completion_tests, 0, 1, 1,
      "*** COMPLETION (nondet) TESTING %s : %%s ****\n" % pkg)

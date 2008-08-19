from fbuild.flxbuild.testprocess import TestProcess

class run_demos(TestProcess):
  help = 'run demonstrations'

  def runme(self, pkg, pkgdict, pkgsummary):
    demos = pkgdict.get("demos",[])
    demos.sort()

    if not demos:
      return

    print "***** DEMO PACKAGE", pkg, "************"

    return self.run_tests(pkg, pkgdict, pkgsummary, demos, 0, 1, 1,
      "*** DEMO PACKAGE %s : %%s ****\n" % pkg)

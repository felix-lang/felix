import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import Tee
from fbuild.path import relativepath, glob_paths

class run_failure_tests(Process):
  help = 'run tests meant to fail'

  def runme(self, pkg, pkgdict, pkgsummary):
    #bad_tests = pkgdict.get("failure_tests",[])
    bad_tests=[]
    bad_tests.sort()

    failed = 0

    root = pkgdict.get('root', '.')
    for testfile in glob_paths(bad_tests):
      log = Tee()
      log.write("**** FAILURE TESTING PACKAGE %s : %s ****\n" % (pkg, testfile))

      #log.write('Running Felix code generator on %s\n' % testfile)
      localpath = relativepath(root, testfile)
      builddir = os.path.join('build', os.path.dirname(localpath))

      try:
        self.shell(os.path.join('bin', 'flxg'),
          '-e',
          '-Ilib',
          '-I' + os.path.dirname(testfile),
          '--cache_dir=' + builddir,
          '--output_dir=' + builddir,
          '--elkhound=' + os.path.join('bin', 'flx_elkhound'),
          '--import=flx.flxh',
          'std',
          os.path.splitext(os.path.basename(testfile))[0],
          log=log,
        )

        #log.write('TESTFILE -- failed as expected %s\n' % testfile)
        self.successes.append((pkg, testfile, log.getvalue()))
      except ExecutionError, e:
        failed = 1

        log.write('TESTFILE -- SUCCEEDED, SHOULD HAVE FAILED! %s\n' % testfile)

        self.failures.append((pkg, testfile, log.getvalue()))

    return not failed

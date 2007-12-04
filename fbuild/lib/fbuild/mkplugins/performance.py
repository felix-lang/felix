import os

from fbuild.flxbuild.process import Process

import config

class performance(Process):
  help = 'make performance tests'

  SPECIAL_TESTS = [
    (os.path.join('bin', 'drivers', 'flx_run'), os.path.join('test', 'drivers', 'flx_run_lib1.flx'),''),
    (os.path.join('test', 'drivers', 'flx_perf_drv1'), os.path.join('test', 'drivers', 'flx_perf_lib1.flx'),'1000'),
  ]

  def performance(self):
    for driver,testfile,moreargs in self.SPECIAL_TESTS:
      test_basename = os.path.splitext(testfile)[0]
      drv_basename = os.path.splitext(driver)[0]
      if config.SUPPORT_DYNAMIC_LOADING:
        testscript = "time "+drv_basename+ " "+test_basename+config.TARGET_CXX.options.EXT_SHLIB+" " + moreargs
        print '(dynamic link) Executing ',testscript
        try:
          self.shell(testscript)
          #print 'TESTFILE -- OK!',testscript
        except ExecutionError, e:
          print 'TESTFILE -- ERROR!',testscript
          raise e

      if config.SUPPORT_STATIC_LINKAGE:
        testscript = "time "+test_basename+config.TARGET_CXX.options.EXT_EXE+" " + moreargs
        print '(static link) Executing ',testscript
        try:
          self.shell(testscript)
          #print 'TESTFILE -- OK!',testscript
        except ExecutionError, e:
          print 'TESTFILE -- ERROR!',testscript
          raise e

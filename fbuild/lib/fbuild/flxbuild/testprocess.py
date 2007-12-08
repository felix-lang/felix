import os
import glob

from fbuild.flxbuild.process import Process, get_latest_src_time, enstamp
from fbuild.flxbuild.package import pkgd, pkgreqs
from fbuild.flxbuild.flxutil import Tee, ExecutionError, filetime, fmtime, mkdirs
from fbuild.path import relativepath, glob_paths

import config

class TestProcess(Process):
  def find_test(self, tl, a, b):
    for x, y, z in tl:
      if x == a and y == b: return True
    return False

  def run_tests(self, pkg, pkgdict, pkgsummary, testfiles, deterministic, static, dynamic, title):
    pkgsummary[(pkg, self.__class__.__name__)] = "started"
    if not testfiles:
      pkgsummary[(pkg, self.__class__.__name__)] = "no tests"
      return 1

    tests_ran = 0
    tests_failed = 0

    # determine the latest time that this package, and all the recurcively dependent
    # packages where last built
    latest_src_time = get_latest_src_time(pkgdict)

    for p in pkgreqs.get(pkg, []):
      latest_src_time = max(latest_src_time, get_latest_src_time(pkgd[p]))

    root = pkgdict.get('root', '.')
    for testfile in glob_paths(testfiles, root):
      localpath = relativepath(root, testfile)

      failed = False

      if self.find_test(self.successes, pkg, testfile): continue
      if self.find_test(self.failures, pkg, testfile): continue

      stamp = os.path.join("pkg-stamps", localpath + '.test')

      # run the tests
      stamptime = filetime(stamp)
      if not self.force and latest_src_time < stamptime:
        self.successes.append((pkg, testfile, "Stamped ok previously on "+fmtime(stamptime)))
      else:
        tests_ran += 1

        log = Tee()
        log.write(title % testfile)
        if not self.quiet:
          if stamptime == 0:
            log.write("+++UNTRIED or FAILED\n")
          elif self.force:
            log.write("+++FORCED\n")
          else:
            log.write("+++OUT OF DATE, source changed since "+fmtime(stamptime)+"\n")

        # build the felix code
        try:
          outbase = self.build_testfile(root, localpath, log)
        except ExecutionError, e:
          failed = True
          log.write('TESTFILE -- ERROR! %s (compiler)\n' % testfile)
        else:
          # run the dynamic tests
          if dynamic and config.SUPPORT_DYNAMIC_LOADING:
            resfilename = outbase + ".resh"

            try:
              driver = self.shell(
                os.path.join('bin', 'flx_pkgconfig'),
                '--path=config',
                '--field=flx_requires_driver',
                '--rec',
                '@' + resfilename,
                log=log,
              )

              driver = driver[0].strip()
              if driver =='': driver = "flx_run"
              driver = os.path.join("bin", driver)
              #log.write("Driver: %s\n" % driver)
              testscript = driver + ' ' + outbase + config.TARGET_CXX.options.EXT_SHLIB

              self.run_test(log, testscript, os.path.splitext(testfile)[0], outbase, deterministic)
            except ExecutionError, e:
              failed = True
              log.write('TESTFILE -- ERROR! %s (dynamic)\n' % testscript)

          # run the static tests
          if static and config.SUPPORT_STATIC_LINKAGE:
            testscript = outbase + config.TARGET_CXX.options.EXT_EXE

            try:
              self.run_test(log, testscript, os.path.splitext(testfile)[0], outbase, deterministic)
            except ExecutionError, e:
              failed = True
              log.write('TESTFILE -- ERROR! %s (static)\n' % testscript)

        if not failed:
          enstamp(stamp, self.quiet)
          self.successes.append((pkg, testfile, log.getvalue()))

      ####

      if failed:
        tests_failed += 1
        self.failures.append((pkg, testfile, log.getvalue()))

      pkgsummary[(pkg, self.__class__.__name__)] = \
        "Passed %d/%d" % (tests_ran - tests_failed, tests_ran)

    return tests_failed == 0


  def build_testfile(self, root, localpath, log):
    testfile = os.path.join(root, localpath)

    optimise_c = self.optimise or "optimise_c" in options
    mode = "std"
    if optimise_c: mode = "Optimised"

    log.write('TRANSLATING %s\n' % testfile)

    buildpath = os.path.join('build', localpath)
    outbase = os.path.splitext(buildpath)[0]
    resfilename = outbase + ".resh"
    incfilename = outbase + ".includes"

    builddir = os.path.dirname(buildpath)
    mkdirs(builddir)

    try:
      self.shell(os.path.join('bin', 'flxg'),
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
    except ExecutionError, e:
      log.write('TESTFILE -- ERROR! %s\n' % testfile)
      raise e

    cflags = self.shell(os.path.join('bin', 'flx_pkgconfig'),
      '--path=config',
      '--field=cflags',
      '@' + resfilename,
      log=log,
    )
    cflags = cflags[0].strip() + " "

    includes = self.shell(os.path.join('bin', 'flx_pkgconfig'),
      '--path=config',
      '--field=includes',
      '@' + resfilename,
      log=log,
    )
    includes = includes[0].strip().split()
    f = open(incfilename,"w")
    for i in includes:
        f.write("#include " + i + "\n")
    f.close()


    #log.write('Compiling generated code of %s\n' % testfile)
    try:
      if config.SUPPORT_DYNAMIC_LOADING:
        log.write("COMPILING GENERATED C++ TEST CODE: %s (dynamic)\n" % mode)
        log.flush()

        dlibs = self.shell(os.path.join('bin', 'flx_pkgconfig'),
          '-r',
          '--path=config',
          '--field=provides_dlib',
          '--field=requires_dlibs',
          '@' + resfilename,
          log=log,
        )
        dlibs = dlibs[0].strip() + " "

        config.TARGET_CXX.build_shared_dll(outbase,
            #outdir='build',
            include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
            optimise=optimise_c,
            debug=self.debug,
            CFLAGS=cflags,
            lib_paths=[config.TARGET_CXX.options.SHLIB_DIR],
            LDFLAGS=dlibs,
            log=log)

      if config.SUPPORT_STATIC_LINKAGE:
        log.write("COMPILING GENERATED C++ TEST CODE: %s (static)\n" % mode)
        log.flush()

        driver = self.shell(os.path.join('bin', 'flx_pkgconfig'),
          '-r',
          '--keeprightmost',
          '--path=config',
          '--field=flx_requires_driver',
          '@' + resfilename,
          log=log,
        )
        driver = driver[0].strip()
        if driver == '': driver = 'flx_run'

        slibs = self.shell(os.path.join('bin', 'flx_pkgconfig'),
          '-r',
          '--keeprightmost',
          '--path=config',
          '--field=provides_slib',
          '--field=requires_slibs',
          driver,
          '@' + resfilename,
          log=log,
        )
        slibs = slibs[0].strip()
        #log.write("slibs=%s\n" % slibs)

        driver = os.path.join(
          config.FLX_RTL_DIR,
          driver + config.TARGET_CXX.options.EXT_STATIC_OBJ,
        )
        #log.write('static driver =%s\n' % driver)

        config.TARGET_CXX.build_felix_static(outbase,
          #outdir='build',
          objects=[driver],
          include_paths=[config.FLX_RTL_DIR, config.FLX_TARGET_CONFIG_DIR],
          optimise=optimise_c,
          debug=self.debug,
          macros=["FLX_STATIC_LINK"],
          CFLAGS=cflags,
          lib_paths=[config.FLX_RTL_DIR],
          libs=[],
          LDFLAGS=slibs,
          log=log)

      #log.write('TESTFILE -- OK! %s\n' % testfile)
    except ExecutionError, e:
      log.write('TESTFILE -- ERROR! %s\n' % testfile)
      raise e

    return outbase


  def run_test(self, log, testscript, inbase, outbase, deterministic):
    log.write('EXECUTING TEST CODE %s\n' % testscript)
    log.flush()

    # FIXME
    argfiles = glob.glob(inbase + '*.args')

    if not argfiles:
      output = self.shell(testscript, verbose=True, log=log)

      f = open(outbase+ '.output', 'w')
      try:
        f.write(''.join(output))
      finally:
        f.close()

      if deterministic:
        output = self.shell(config.DIFF,
          inbase + '.expect',
          outbase + '.output',
          verbose=True,
          log=log)

    else:
      for argcount, argfile in zip(range(len(argfiles)), argfiles):
        # read the arguments fromm the file
        f = open(argfile)
        try:
          args = f.read().strip()
        finally:
          f.close()

        output = self.shell(testscript, args, verbose=True, log=log)

        f = open('%s-%s.argoutput' % (outbase, argcount), 'w')
        try:
          f.write(''.join(output))
        finally:
          f.close()

        if deterministic:
           output = self.shell(config.DIFF,
            '%s-%s.argexpect' % (inbase, argcount),
            '%s-%s.argoutput' % (outbase, argcount),
            quiet=True,
            log=log,
          )

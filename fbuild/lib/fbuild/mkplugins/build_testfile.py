import os

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs, Tee, ExecutionError

import config

class build_testfile(Process):
  def runme(self, root, localpath, log):
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

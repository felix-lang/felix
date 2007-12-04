import os
import sys
import glob

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import erasefile

import config

class virgin(Process):
  help = 'clean and remove all the sources'

  def runme(self, *args):
    for d in [ \
        os.path.join("judy", "JudyCommon"),
        os.path.join("judy", "Judy1"),
        os.path.join("judy", "JudyL"),
        os.path.join("judy", "JudySL"),
        os.path.join("judy", "JudyHS"),
        "judy",
        os.path.join("bagley", "data"),
        os.path.join("bagley", "felix"),
        "bagley",
        "bin",
        "build",
        os.path.join("cpkgs", "build"),
        os.path.join("cpkgs", "host"),
        os.path.join("cpkgs", "target"),
        "cpkgs",
        os.path.join("demos", "faio"),
        os.path.join("demos", "sdl"),
        "demos",
        "demux",
        os.path.join("doc", "elkhound"),
        os.path.join("doc", "flx"),
        os.path.join("doc", "flxcc"),
        os.path.join("doc", "gmp"),
        os.path.join("doc", "grammar"),
        os.path.join("doc", "gsl"),
        os.path.join("doc", "htmlman"),
        os.path.join("doc", "impldoc"),
        os.path.join("doc", "mmap"),
        os.path.join("doc", "opengl"),
        os.path.join("doc", "refman"),
        os.path.join("doc", "rtl"),
        os.path.join("doc", "sdl"),
        os.path.join("doc", "test"),
        os.path.join("doc", "tutorial"),
        "doc",
        os.path.join("dypgen", "generators", "dypgen"),
        os.path.join("dypgen", "generators", "pgen"),
        os.path.join("dypgen", "generators"),
        os.path.join("dypgen", "dyplib"),
        "dypgen",
        "elk",
        "faio",
        "hostlib",
        "impldoc",
        os.path.join("lib", "gsl"),
        os.path.join("lib", "mpi"),
        os.path.join("lib", "pari"),
        os.path.join("lib", "GL"),
        os.path.join("lib", "SDL"),
        "lib",
        os.path.join("man", "man1"),
        "man",
        "manifests",
        os.path.join("meta", "godi"),
        os.path.join("meta", "godiva"),
        "meta",
        os.path.join("misc", "vim"),
        os.path.join("misc", "lua"),
        os.path.join("misc", "jedit"),
        "misc",
        "lpsrc-cache",
        "oldebian",
        "pkg",
        os.path.join("pkg-stamps", "test", "regress"),
        os.path.join("pkg-stamps", "test"),
        os.path.join("pkg-stamps", "tut", "embedding"),
        os.path.join("pkg-stamps", "tut", "macros"),
        os.path.join("pkg-stamps", "tut", "migration"),
        os.path.join("pkg-stamps", "tut", "tutorial"),
        os.path.join("pkg-stamps", "tut"),
        "pkg-stamps",
        "pthread",
        "rtl",
        "script",
        os.path.join("speed", "specs"),
        os.path.join("speed", "src", "ada"),
        os.path.join("speed", "src", "c"),
        os.path.join("speed", "src", "felix"),
        os.path.join("speed", "src", "haskell"),
        os.path.join("speed", "src", "java"),
        os.path.join("speed", "src", "ocaml"),
        os.path.join("speed", "src", "pascal"),
        os.path.join("speed", "src"),
        os.path.join("speed", "xlators"),
        "speed",
        "spkgs",
        "src",
        os.path.join("src", "cil"),
        os.path.join("test", "drivers"),
        os.path.join("test", "faio"),
        os.path.join("test", "glob"),
        os.path.join("test", "gmp"),
        os.path.join("test", "mmap"),
        os.path.join("test", "pthread"),
        os.path.join("test", "regress"),
        os.path.join("test", "stdlib"),
        os.path.join("test", "tre"),
        "test",
        "tmp",
        os.path.join("tools", "lua"),
        "tools",
        "tre",
        os.path.join("tut", "embedding"),
        os.path.join("tut", "examples"),
        os.path.join("tut", "macros"),
        os.path.join("tut", "migration"),
        os.path.join("tut", "sdl"),
        os.path.join("tut", "tutorial"),
        "tut",
        "unixem",
        "www",
        ]:
      for f in glob.glob(os.path.join(d, "*")):
        print "Del",f
        erasefile(f)
      try:
        if os.path.exists(d):
          os.rmdir(d)
          print "Rmdir",d
      except:
        print "FAILED Rmdir",d
    for f in glob.glob(os.path.join(config.FLX_LPARCHIVE, "lpsrc", "*.cache")):
      erasefile(f)

    self.shell(
      config.ICSR,
      '--force',
      '--break-on-error',
      '"%s"' % os.path.join(config.FLX_LPARCHIVE, 'lpsrc', 'flx_config.pak'),
    )
    sys.exit(0)

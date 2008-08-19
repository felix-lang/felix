import os
import shutil

from fbuild.flxbuild.process import Process
from fbuild.flxbuild.flxutil import mkdirs

import config

class mkdoc(Process):
  def __init__(self, *args, **kwds):
    super(mkdoc, self).__init__(*args, **kwds)

    self.already_generated = {}

  def runme(self, pkg, pkgdict, *args):
    mkdirs("pkg-stamps")

    weaver_directory = pkgdict.get("weaver_directory","")
    iscr_source = pkgdict.get("iscr_source",[])
    if iscr_source and weaver_directory:
      print "GENERATING DOCUMENTATION",weaver_directory

      iscr_source = filter(lambda x: x not in self.already_generated, iscr_source)

      mkdirs(weaver_directory)
      shutil.copy(os.path.join('misc', 'plus.gif'),        weaver_directory)
      shutil.copy(os.path.join('misc', 'minus.gif'),       weaver_directory)
      shutil.copy(os.path.join('misc', 'dot.gif'),         weaver_directory)
      shutil.copy(os.path.join('misc', 'interscript.css'), weaver_directory)
      shutil.copy(os.path.join('misc', 'user.css'),        weaver_directory)

      for src in iscr_source:
        self.shell(config.ISCR,
          '--language=en',
          '--weaver=web',
          '--weaver=latex',
          '--passes=2',
          '--weaver-directory=' + weaver_directory,
          os.path.join(config.FLX_LPARCHIVE, src),
        )

        self.already_generated[src] = 1

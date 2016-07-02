
import fbuild
import fbuild.builders.file
import os, platform
from fbuild.path import Path
from fbuild.builders.file import copy

# ------------------------------------------------------------------------------

def build(phase, felix):
    print ("BUILDING PLUGINS")
    plugins = [
      #"ocaml2html",
      #"py2html",
      #"fdoc2html",
      #"flx2html",
      #"cpp2html",
      #"fpc2html",
      #"fdoc_slideshow",
      #"fdoc_paragraph",
      #"fdoc_heading",
      #"fdoc_fileseq",
      #"fdoc_scanner",
      #"fdoc_button",
      ]

    plat = platform.system()

    # queue up only the relevant plugins
    if plat == 'Windows':
      plugins.append("toolchain_msvc_win32")

    elif plat == 'Linux':
      plugins.append("toolchain_clang_linux")
      plugins.append("toolchain_gcc_linux")

    else: # osx/bsd
      plugins.append("toolchain_clang_osx")
      plugins.append("toolchain_gcc_osx")

    plugins.append("flx_plugin")

    for base in plugins:
      shlib = felix.compile(phase.ctx.buildroot/('share/lib/plugins/'+base+'.flx'),flags=['-od',phase.ctx.buildroot/'host/lib/rtl'])


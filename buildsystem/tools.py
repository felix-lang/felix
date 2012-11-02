import fbuild
import fbuild.builders.file
import os
# ------------------------------------------------------------------------------

def build(phase, felix):

    try:
        exe = felix.compile(phase.ctx.buildroot/'wiki/wiki.flx', static=True)
        fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    except:
        print("Warning : wiki not built. Continuing..." )

    exes = [
      "flx_grep",
      "flx_replace",
      "flx_ls",
      "flx_cp",
      "webserver",
      "flx_gramdoc",
      "flx_libcontents",
      "flx_libindex",
      "norK",
      "rentut",
      "mktutindex",
      "flx_perror",
      ]

    for base in exes:
      try:
          exe = felix.compile(phase.ctx.buildroot/('tools/'+base+'.flx'), static=True)
          fbuild.builders.file.copy(phase.ctx, exe, 'bin')
      except:
          print("Warning : "+base+" not built. Continuing..." )

    try:
      os.mkdir(phase.ctx.buildroot/'shlib')
    except:
      pass

    plugins = [
      "ocaml2html",
      "py2html",
      "fdoc2html",
      "flx2html",
      "cpp2html",
      "fpc2html",
      "fdoc_slideshow",
      "fdoc_paragraph",
      "fdoc_heading",
      "fdoc_fileseq",
      "fdoc_scanner",
      ]
    for base in plugins:
      try:
          shlib = felix.compile(phase.ctx.buildroot/('tools/'+base+'.flx'))
          fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')
      except:
          print("Warning : " + base + " not built. Continuing..." )



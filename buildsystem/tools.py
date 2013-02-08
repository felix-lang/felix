import fbuild
import fbuild.builders.file
import os
# ------------------------------------------------------------------------------

def build(phase, felix):

    
    # The wiki is killing the compiler, inlining procedure
    # cannot cope. Possible inline explosion.
    # Need to investigate this. Maybe its too big,
    # and maybe the small change I just introduced was enough
    # to kill it.

    #try:
    #    exe = felix.compile(phase.ctx.buildroot/'wiki/wiki.flx', static=True)
    #    fbuild.builders.file.copy(phase.ctx, exe, 'bin')
    #except:
    #    print("Warning : wiki not built. Continuing..." )

    exes = [
      "flx_grep",
      "flx_replace",
      "flx_ls",
      "flx_cp",
      "webserver",
      "flx_gramdoc",
      "flx_libcontents",
      "flx_libindex",
      "flx_renumber",
      "flx_mktutindex",
      "flx_perror",
      "flx_tangle",
      "flx_gengraph",
      ]

    optional_exes = [
      "norK",
      ]

    for base in exes:
      exe = felix.compile(phase.ctx.buildroot/('tools/'+base+'.flx'), static=True)
      fbuild.builders.file.copy(phase.ctx, exe, 'bin')

    for base in optional_exes:
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
      "fdoc_button",
      ]
    for base in plugins:
      shlib = felix.compile(phase.ctx.buildroot/('tools/'+base+'.flx'))
      fbuild.builders.file.copy(phase.ctx, shlib, 'shlib')



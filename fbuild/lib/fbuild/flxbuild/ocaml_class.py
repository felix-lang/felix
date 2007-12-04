# discover which ocaml compilers are available
import os
import sys
import shutil

from fbuild.flxbuild.flxutil import mkdirs, ExecutionError
from fbuild.flxbuild.compiler_base import compiler_base

class ocaml(compiler_base):

  def autodetect(self, linkopts, log=None):
    opt = self.options

    if os.name == 'nt' or sys.platform == 'cygwin':
      opt.EXT_EXE = '.exe'
      # ocamlbuild doesn't work under windows
      opt.HAVE_OCAMLBUILD = False
    else:
      opt.EXT_EXE = ''

      try: # check if ocamlbuild exists
        self.shell('ocamlbuild', '-version', log=log)
        opt.OCAMLBUILD = 'ocamlbuild -classic-display'
        #opt.HAVE_OCAMLBUILD = True
        # ocaml build doesn't work properly at the moment
        # due to hygiene checks or something .. so i have
        # to disable it
        opt.HAVE_OCAMLBUILD = False
      except ExecutionError:
        opt.HAVE_OCAMLBUILD = False

    try: # check if the client bootstrapped the native code compiler
      self.shell('ocamlopt.opt', log=log)
      opt.OCAMLCC = 'ocamlopt.opt' + (linkopts and ' ' + linkopts or '')
      opt.OCAMLLEX = 'ocamllex.opt'
      opt.OCAMLYACC = 'ocamlyacc'
      opt.NATIVE_CODE_COMPILER = True
    except ExecutionError:
      try: # check if the client has unbootstrapped native code compiler
        self.shell('ocamlopt', log=log)
        opt.OCAMLCC = 'ocamlopt' + (linkopts and ' ' + linkopts or '')
        opt.OCAMLLEX = 'ocamllex'
        opt.OCAMLYACC = 'ocamlyacc'
        opt.NATIVE_CODE_COMPILER = True
      except ExecutionError:
        opt.NATIVE_CODE_COMPILER = False
        try: # check if the client has ocaml at all ..
          self.shell('ocamlc', log=log)
          opt.OCAMLCC = 'ocamlc'
          opt.OCAMLLEX = 'ocamllex'
          opt.OCAMLYACC = 'ocamlyacc'
          opt.OCAMLDOC = 'ocamldoc'
        except ExecutionError:
          print "WARNING: CANT FIND OCAML TOOLS (ocamlc, ocamllex, ocamlyacc)"
          opt.OCAMLCC = 'false ocamlc'
          opt.OCAMLLEX = 'false ocamllex'
          opt.OCAMLYACC = 'false ocamlyacc'

    try: # check if there is a native code version of the bytecode compiler
      self.shell('ocamlc.opt', log=log)
      opt.OCAMLB = 'ocamlc.opt'
    except ExecutionError:
      try: # check if the client has ocaml at all
        self.shell('ocamlc', log=log)
        opt.OCAMLB = 'ocamlc'
      except ExecutionError:
        print "WARNING: CANT FIND OCAML TOOL 'ocamlc'"
        opt.OCAMLB = 'false ocamlc'

    try:
      self.shell('ocamldoc.opt', log=log)
      opt.OCAMLDOC = 'ocamldoc.opt'
    except ExecutionError:
      try:
        self.shell('ocamldoc', log=log)
        opt.OCAMLDOC = 'ocamldoc'
      except ExecutionError:
        print "WARNING: CANT FIND OCAML TOOL 'ocamldoc'"
        opt.OCAMLDOC = 'false ocamldoc'

    # set the default ocaml compiler
    if opt.OCAMLCC[0] != '#':
      opt.OCAMLC = opt.OCAMLCC
    else:
      opt.OCAMLC = opt.OCAMLB

    opt.OCAMLCP = "ocamlcp"
    # where the ocaml is installed
    try:
      output = self.shell(opt.OCAMLC, '-where', verbose=False)
    except ExecutionError:
      print "Woops, Can't run", opt.OCAMLC
      sys.exit(1)
    opt.OCAML_INCLUDE_DIRECTORY= output[0].strip()

    try:
      output = self.shell(opt.OCAMLB, '-v', verbose=False)
    except ExecutionError:
      print "Woops, Can't run", opt.OCAMLB
      sys.exit(1)
    opt.OCAMLB_DESCRIPTION, ocamlb_version = output[0].strip().split(', ')

    try:
      output = self.shell(opt.OCAMLC, '-v', verbose=False)
    except ExecutionError:
      print "Woops, Can't run", opt.OCAMLC
      sys.exit(1)
    opt.OCAMLC_DESCRIPTION, ocamlc_version = output[0].strip().split(', ')
    if ocamlb_version != ocamlc_version:
      print "Inconsistent Ocaml tool versions"
      print "Ocaml: bytecode compiler", ocamlb_version
      print "Ocaml: compiler         ", ocamlc_version
      sys.exit(1)
    else:
      opt.OCAML_VERSION = ocamlb_version

    warn=' -w yzex -warn-error FDPSU'
    f = open('tmp'+os.sep+'camldummy.ml','w')
    try:
      f.write('print_endline "OK";;\n')
    finally:
      f.close()

    try:
      self.shell(opt.OCAMLC,
        warn,
        os.path.join('tmp', 'camldummy.ml'),
        verbose=False,
      )
    except ExecutionError:
      pass
    else:
      opt.OCAMLB = opt.OCAMLB + warn
      opt.OCAMLC = opt.OCAMLC + warn

    try:
      self.shell(opt.OCAMLC, '-g',
        os.path.join('tmp', 'camldummy.ml'),
        verbose=False,
      )
    except ExecutionError:
      opt.OCAMLC_SUPPORTS_DEBUG = False
    else:
      opt.OCAMLC_SUPPORTS_DEBUG = True

  def report_config(self):
    opt = self.options

    print
    print "OCAML tool configuration"
    print "************************"
    print
    print "Ocaml Version",opt.OCAML_VERSION
    if opt.NATIVE_CODE_COMPILER:
      print "Using Native code Compiler"
    else:
      print "Using Bytecode Compiler"
    print "Lexer generator .............. ",opt.OCAMLLEX
    print "Parser generator ............. ",opt.OCAMLYACC
    print "Bytecode and Interface compiler",opt.OCAMLB
    print "   ",opt.OCAMLB_DESCRIPTION
    print "Compiler ..................... ",opt.OCAMLC
    print "   ",opt.OCAMLC_DESCRIPTION
    print "Profiling Compiler ........... ",opt.OCAMLCP
    print "Document Generator ........... ",opt.OCAMLDOC
    print "C include directory .......... ",opt.OCAML_INCLUDE_DIRECTORY
    print


  def compile_thing(self, COM, EXT_SRC, EXT_DST, basenames,
      outdir='',
      include_paths=[],
      packs=[],
      profile=False,
      optimise=False,
      FLAGS=None,
      log=None,
      ):
    objects = []
    for basename in basenames:
      src = self.find_in_flx_dir(basename + EXT_SRC)
      dst = os.path.join(outdir, basename + EXT_DST)

      # search for generated files first
      f = os.path.join(outdir, src)
      if os.path.exists(f):
        src = f

      mkdirs(os.path.dirname(dst))

      cmd = [COM]
      for i in include_paths: cmd.extend(('-I', i))
      cmd.extend(('-I', os.path.dirname(dst)))

      if optimise: cmd.extend(('-unsafe', '-noassert'))

      for path, packed, parts in packs:
        if basename in parts:
          cmd.extend(('-for-pack', pack))

      cmd.extend(('-o', dst))
      cmd.extend(('-c', src))

      # compile it
      self.shell(*cmd, **dict(log=log))

      objects.append(dst)

    return objects


  def compile_bytecode_thing(self, *args, **kwds):
    if kwds.pop('profile', False):
      compiler = self.options.OCAMLCP
    else:
      compiler = self.options.OCAMLB

    if kwds.pop('debug', False): compiler += ' -g'

    return self.compile_thing(compiler, *args, **kwds)


  def compile_native_thing(self, *args, **kwds):
    if not self.options.NATIVE_CODE_COMPILER:
      return self.compile_bytecode_thing(*args, **kwds)

    compiler = self.options.OCAMLC
    if kwds.pop('profile', False):
      compiler += ' -p'

    if kwds.get('optimise', False):
      compiler += ' -inline 5'

    if kwds.pop('debug', False) and self.options.OCAMLC_SUPPORTS_DEBUG:
      compiler += ' -g'

    return self.compile_thing(compiler, *args, **kwds)

  ####

  def compile_interface(self, *args, **kwds):
    return self.compile_bytecode_thing('.mli', '.cmi', *args, **kwds)

  def compile_bytecode(self, *args, **kwds):
    return self.compile_bytecode_thing('.ml', '.cmo', *args, **kwds)

  def compile_native(self, *args, **kwds):
    return self.compile_native_thing('.ml', '.cmx', *args, **kwds)

  def compile_module(self, *args, **kwds):
    if kwds.pop('bytecode', True):
      return self.compile_bytecode(*args, **kwds)
    else:
      return self.compile_native(*args, **kwds)

  ####

  def link_thing(self, LINK, EXT_DST, objects, outfile,
      outdir='',
      include_paths=[],
      libs=[],
      log=None,
      ):
    dst = os.path.join(outdir, outfile)
    mkdirs(os.path.dirname(dst))

    cmd = [LINK]
    for i in include_paths: cmd.extend(('-I', i))
    cmd.extend(('-I', os.path.dirname(dst)))

    cmd.extend(('-o', dst))
    cmd.extend([lib + EXT_DST for lib in libs])
    cmd.extend(objects)

    self.shell(*cmd, **dict(log=log))

    return dst

  def link_bytecode_lib(self, objects, outfile, *args, **kwds):
    return self.link_thing(
        self.options.OCAMLB + ' -a', '.cma', objects, outfile + '.cma',
        *args, **kwds)

  def link_native_lib(self, objects, outfile, *args, **kwds):
    if not self.options.NATIVE_CODE_COMPILER:
      return self.link_bytecode(*args, **kwds)

    return self.link_thing(
        self.options.OCAMLC + ' -a', '.cmxa', objects, outfile + '.cmxa',
        *args, **kwds)

  def link_lib(self, *args, **kwds):
    if kwds.pop('bytecode', True):
      return self.link_bytecode_lib(*args, **kwds)
    else:
      return self.link_native_lib(*args, **kwds)

  def link_bytecode_exe(self, *args, **kwds):
    opt = self.options
    return self.link_thing(opt.OCAMLC, '.cma', *args, **kwds)

  def link_native_exe(self, *args, **kwds):
    if not self.options.NATIVE_CODE_COMPILER:
      return self.link_bytecode_exe(*args, **kwds)

    opt = self.options
    return self.link_thing(opt.OCAMLC, '.cmxa', *args, **kwds)

  def link_exe(self, *args, **kwds):
    if kwds.pop('bytecode', True):
      return self.link_bytecode_exe(*args, **kwds)
    else:
      return self.link_native_exe(*args, **kwds)

  ####

  def gen_thing(self, COM, EXT_SRC, basenames,
      outdir='',
      FLAGS=[],
      log=None):
    for basename in basenames:
      src = self.find_in_flx_dir(basename + EXT_SRC)
      dst = os.path.join(outdir, basename + EXT_SRC)

      mkdirs(os.path.dirname(dst))
      shutil.copy(src, dst)

      cmd = [COM]
      cmd.extend(FLAGS)
      cmd.append(dst)

      self.shell(*cmd, **dict(log=log))


  def gen_lexer(self, *args, **kwds):
    return self.gen_thing(self.options.OCAMLLEX, '.mll', *args, **kwds)


  def gen_parser(self, *args, **kwds):
    return self.gen_thing(self.options.OCAMLYACC, '.mly',
        *args, **kwds)


  def gen_pgen_parser(self, *args, **kwds):
    return self.gen_thing(os.path.join('bin', 'pgen'), '.dyp',
        *args, **kwds)

  def gen_dypgen_parser(self, *args, **kwds):
    return self.gen_thing(os.path.join('bin', 'dypgen'), '.dyp',
        *args, **kwds)

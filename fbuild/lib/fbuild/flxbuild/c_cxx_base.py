import os
import sys

from fbuild.flxbuild.flxutil import mkdirs, ExecutionError
from fbuild.flxbuild.compiler_base import compiler_base

class c_cxx_base(compiler_base):
  DEFAULT_COM = None
  DEFAULT_AR = None
  DEFAULT_RANLIB = None

  def set_options(self,
      COM=None,
      AR=None,
      RANLIB=None,
      build="posix",
      model="detect",
      use="generic",
      include_paths=None,
      lib_paths=None,
      **kwds):
    super(c_cxx_base, self).set_options(**kwds)

    opt = self.options

    # RF: this looks like in most cases it replaces None with None...
    if COM is None:           COM = self.DEFAULT_COM
    if AR is None:            AR = self.DEFAULT_AR
    if RANLIB is None:        RANLIB = self.DEFAULT_RANLIB
    if include_paths is None: include_paths = []
    if lib_paths is None:     lib_paths = []

    assert COM, 'need to specify a compiler'
    assert AR, 'need to specify a static library linker'

    opt.COM = COM
    opt.AR = AR
    opt.RANLIB = RANLIB
    opt.build = build
    opt.model = model
    opt.use = use
    opt.include_paths = include_paths
    opt.lib_paths = lib_paths

    # defaults for gcc specific stuff
    opt.HAVE_GNU_X86 = False
    opt.HAVE_GNU_X86_64 = False
    opt.USE_REGPARM3 = False
    opt.HAVE_CGOTO = False
    opt.HAVE_ASM_LABELS = False
    opt.HAVE_STL_GNU_CXX = False
    opt.HAVE_GNU_BUILTIN_EXPECT = False

    opt.HAVE_PIC = False
    opt.HAVE_STATIC_OPENMP = False
    opt.HAVE_SHARED_OPENMP = False

    print "COM =", opt.COM

  ########

  # RF: would be nice to know which kind of source was being written
  def write_src(self, data, basename=os.path.join('tmp', 'tmp')):
    mkdirs(os.path.dirname(basename))

    f = open(basename + self.options.EXT_SRC_MAIN, "w")
    try:
      f.write(data + '\n')
    finally:
      f.close()

    return basename

  # RF: temporarily added this to get the dll building config tests working.
  # previously write_src was creating .cpp files and the build_dll code
  # was looking for .cxx. This is a work around.
  def write_lib_src(self, data, basename=os.path.join('tmp', 'tmp')):
    mkdirs(os.path.dirname(basename))

    f = open(basename + self.options.EXT_SRC_LIB, "w")
    try:
      f.write(data + '\n')
    finally:
      f.close()

    return basename

  ########

  def compile_thing(self, COM, EXT_SRC, EXT_DST, basenames,
      outdir='',
      include_paths=[],
      macros=[],
      optimise=False,
      debug=False,
      CFLAGS=None,
      log=None,
      ):
    opt = self.options

    objects = []
    for basename in basenames:
      src = self.find_in_flx_dir(basename + EXT_SRC)
      dst = os.path.join(outdir, basename + EXT_DST)
      mkdirs(os.path.dirname(dst))

      # compile for a dll: position independent code etc
      cmd = [COM]

      # debug symbols
      if debug: cmd.append(opt.DEBUG_FLAGS)

      # optimisation
      if optimise: cmd.append(opt.OPTIMISE)

      # include path
      for i in include_paths:     cmd.extend((opt.SPEC_INCLUDE, i))
      for i in opt.include_paths: cmd.extend((opt.SPEC_INCLUDE, i))

      # output file
      cmd.append(opt.SPEC_OBJ_FILENAME + dst)

      #macros
      for i in macros: cmd.append(opt.SPEC_DEFINE + i)

      if opt.use == "host":     cmd.append(opt.SPEC_DEFINE + "HOST_BUILD")
      elif opt.use == "target": cmd.append(opt.SPEC_DEFINE + "TARGET_BUILD")

      if CFLAGS: cmd.append(CFLAGS)

      #input file
      cmd.append(src)

      # compile it
      self.shell(*cmd, **dict(log=log))

      objects.append(dst)

    return objects

  ####

  def compile_static_thing(self, COM, EXT_SRC, *args, **kwds):
    if self.options.HAVE_STATIC_OPENMP: COM += ' ' + self.options.OPENMP

    return self.compile_thing(COM, EXT_SRC, self.options.EXT_STATIC_OBJ,
        *args, **kwds)


  # compile a file to an object suitable for inclusion in a static link
  # version of the RTL
  def compile_static_rtl(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_STATIC_RTL', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_static_thing(COM, opt.EXT_SRC_LIB, *args, **kwds)


  def compile_static_main(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_STATIC_MAIN', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_static_thing(COM, opt.EXT_SRC_MAIN, *args, **kwds)


  def compile_felix_static(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_STATIC_FLX', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_static_thing(COM, opt.EXT_SRC_LIB, *args, **kwds)

  ####

  def compile_shared_thing(self, COM, EXT_SRC, *args, **kwds):
    if self.options.HAVE_PIC: COM += ' ' + self.options.PIC

    # JS: openmp support now detected, and the right compiler option applied
    # Windows prof. ed. 2005 supports openMP for dynamic link only
    # Express doesn't support openMP
    # Xbox also supports static link, but we don't handle that here
    #
    # So far, we don't link the right library.. can't see at present how
    # to get the library in, and not sure that /openmp doesn't link it
    # automatically.
    # It's called vcomp.lib/vcomp.dll.
    # this has to specified with LDFLAGS to go after the 'link' directive
    # assuming the /openmp switch doesn't force the right linkage

    if self.options.HAVE_SHARED_OPENMP: COM += ' ' + self.options.OPENMP

    # add shared flag, then execute
    return self.compile_thing(COM, EXT_SRC, self.options.EXT_SHARED_OBJ,
        *args, **kwds)


  # compile a file to an object suitable for inclusion in a mainline
  # which links to shared libraries
  def compile_shared_rtl(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_DYNAMIC_RTL', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_shared_thing(COM, opt.EXT_SRC_LIB, *args, **kwds)


  def compile_shared_main(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_DYNAMIC_MAIN', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_shared_thing(COM, opt.EXT_SRC_MAIN, *args, **kwds)


  def compile_felix_dll(self, *args, **kwds):
    opt = self.options
    COM = opt.__dict__.get('CCOBJ_DYNAMIC_FLX', opt.COM + ' ' + opt.SPEC_COMPILE_OBJ)
    return self.compile_shared_thing(COM, opt.EXT_SRC_LIB, *args, **kwds)

  ########

  def link_thing(self, LINK, EXT_DST, objects, outfile,
      lib_paths=[],
      libs=[],
      LDFLAGS="",
      log=None,
      ):
    dst = outfile + EXT_DST

    opt = self.options
    cmd = [LINK]
    cmd.append(opt.SPEC_EXE_FILENAME + dst)
    cmd.extend(objects)

    # RF: this is a hack to make sure that /link come before all link flags
    # under visual studio. I tried making sure that LDFLAGS had /link first
    # and moved it to just after the obj files, but that broke the gcc build
    # as some folk pass the libs in "libs" and others pass them directly
    # as LDFLAGS args. So much for encapsulation, eh?
    if opt.PRE_LINK_FLAGS: cmd.append(opt.PRE_LINK_FLAGS)

    for i in lib_paths:     cmd.append(opt.SPEC_LIBPATH + i)
    for i in opt.lib_paths: cmd.append(opt.SPEC_LIBPATH + i)

    for i in libs:
      if opt.POSIX and i.startswith('lib'):
        i = i[3:]

      cmd.append(opt.SPEC_LIB + i)

    cmd.append(LDFLAGS)

    mkdirs(os.path.dirname(outfile))

    self.shell(*cmd, **dict(log=log))

    return dst

  ####

  def link_static_thing(self, COM, EXT_DST, *args, **kwds):
    return self.link_thing(COM, EXT_DST, *args, **kwds)


  def link_static_rtl(self, objects, outfile,
      lib_paths=[],
      libs=[],
      LDFLAGS="",
      log=None):
    opt = self.options

    filename = outfile + opt.EXT_LIB

    mkdirs(os.path.dirname(outfile))

    self.shell(opt.AR, opt.SPEC_AR_OUT_FILENAME + filename,
      *objects, **dict(log=log))

    if opt.RANLIB:
      self.shell(opt.RANLIB, filename, log=log)

    return filename


  def link_static_program(self, *args, **kwds):
    opt = self.options
    return self.link_static_thing(opt.CCLINK_STATIC, opt.EXT_EXE, *args, **kwds)

  ####

  def link_shared_thing(self, COM, EXT_DST, *args, **kwds):
    return self.link_thing(COM, EXT_DST, *args, **kwds)


  def link_shared_rtl(self, *args, **kwds):
    opt = self.options
    return self.link_shared_thing(opt.CCLINK_DYNAMIC_RTL, opt.EXT_DYLIB,
        *args, **kwds)


  def link_shared_dll(self, *args, **kwds):
    opt = self.options
    return self.link_shared_thing(opt.CCLINK_DYNAMIC_FLX, opt.EXT_SHLIB,
        *args, **kwds)


  def link_shared_program(self, *args, **kwds):
    opt = self.options
    return self.link_shared_thing(opt.CCLINK_DYNAMIC_MAIN, opt.EXT_EXE,
        *args, **kwds)

  ########

  def build_thing(self, compile_fn, link_fn, basenames,
      outfile=None,
      outdir='',
      objects=[],
      include_paths=[],
      macros=[],
      optimise=False,
      debug=False,
      CFLAGS="",
      lib_paths=[],
      libs=[],
      LDFLAGS="",
      log=None,
      ):
    if type(basenames) != type([]):
      basenames = [basenames]

    assert basenames

    objects = objects + compile_fn(basenames,
            outdir=outdir,
            include_paths=include_paths,
            macros=macros,
            optimise=optimise,
            debug=debug,
            CFLAGS=CFLAGS,
            log=log,
            )

    if outfile is None:
      outfile = basenames[0]

    return link_fn(objects, outfile,
        lib_paths=lib_paths,
        libs=libs,
        LDFLAGS=LDFLAGS,
        log=log,
      )


  def build_static_rtl(self, *args, **kwds):
    return self.build_thing(self.compile_static_rtl, self.link_static_rtl,
        *args, **kwds)

  def build_felix_static(self, *args, **kwds):
    return self.build_thing(self.compile_felix_static, self.link_static_program,
        *args, **kwds)

  def build_static_program(self, *args, **kwds):
    return self.build_thing(self.compile_static_main, self.link_static_program,
        *args, **kwds)

  def build_shared_rtl(self, *args, **kwds):
    return self.build_thing(self.compile_shared_rtl, self.link_shared_rtl,
        *args, **kwds)

  def build_shared_dll(self, *args, **kwds):
    return self.build_thing(self.compile_felix_dll, self.link_shared_dll,
        *args, **kwds)

  def build_shared_program(self, *args, **kwds):
    return self.build_thing(self.compile_shared_main, self.link_shared_program,
        *args, **kwds)

  ########

  def run_static_program(self, *args, **kwds):
    filename = self.build_static_program(*args, **kwds)
    return self.shell(filename)

  def run_shared_program(self, *args, **kwds):
    filename = self.build_shared_program(*args, **kwds)
    return self.shell(filename)

  ########

  def run_static_string_program(self, data, basename, *args, **kwds):
    basename = self.write_src(data, basename)
    return self.run_static_program(basename, *args, **kwds)


  def run_shared_string_program(self, data, basename, *args, **kwds):
    basename = self.write_src(data, basename)
    return self.run_shared_program(basename, *args, **kwds)

  ########

  def build_string_program(self, data, basename='tmp', **kwds):
    filename = self.write_src(data, basename)
    return self.build_static_program(filename, **kwds)

  ####

  def compile_dummy_main(self, **kwds):
    basename = 'tmp' + os.sep + 'dummy'
    filename = self.write_src(
        'int main(int argc, char** argv) { return 0; }', basename)
    return self.compile_static_main([filename], **kwds)


  def compile_dummy_lib_program(self, **kwds):
    basename = 'tmp' + os.sep + 'dummy_lib'
    # RF: This can be compiled as both c and c++ these days it seems
    # hence conditional extern "C"
    # P.S. This lovingly hand crafted function doesn't seem to be called
    sys.exit(1234) # let's see!
    proggy = """
#ifdef __cplusplus
extern "C"
#endif
int fred(int argc, char** argv) { return 0; }
"""

    filename = self.write_src(proggy, basename)

    return self.build_static_rtl([filename], **kwds)

  ########

  def check_macro_defined(self, macro, header=''):
    if header:
      header = '#include <%s>' % header

    filename = "tmp" + os.sep + "mchk"
    self.write_src("""
%s
#ifndef %s
#error %s
#endif
int main(int argc, char** argv) {return 0;}
""" % (header, macro, macro), filename)

    try:
      self.compile_static_main([filename])
    except ExecutionError:
      if header:
        print "NOT defined %s in %s" % (macro, header)
      else:
        print "NOT defined", macro
      return False
    else:
      if header:
        print "#defined %s in %s" % (macro, header)
      else:
        print "#defined", macro
      return True


  def check_header_exists(self, name):
    basename = "tmp" + os.sep + "hchk"
    filename = self.write_src('#include <%s>\nint main(int argc, char** argv) { return 0; }' % name, basename)
    try:
      self.compile_static_main([filename])
      print "#include <%s>" % name
      return True
    except ExecutionError:
      print "NO HEADER <%s>" % name
      return False

  ########

  def get_type_size(self, typedef, header=''):
    if header:
      header = '#include <%s>' % header

    if typedef[:7] != 'typedef':
      t = 'typedef %s t;' % typedef
    else:
      t = typedef

    filename = "tmp" + os.sep + "type_size"
    self.write_src("""
#include <stddef.h>
#include <stdio.h>
%s

%s

int main(int argc, char** argv) {
  printf("%%d\\n",(int)sizeof(t));
  return 0;
}
""" % (header, t), filename)

    try:
      lines = self.run_static_program(filename)
    except ExecutionError:
      return None

    size = int(lines[0])
    return size


  def get_type_align(self, typedef, header=''):
    """the typedef defines alias 't' for the type"""
    if header:
      header = '#include <%s>' % header

    if typedef[:7] != 'typedef':
      t = 'typedef %s t;' % typedef
    else:
      t = typedef

    filename = "tmp" + os.sep + "type_align"
    self.write_src("""
#include <stddef.h>
#include <stdio.h>
%s

%s

struct TEST {
  char c;
  t mem;
};

int main(int argc, char** argv) {
  printf("%%d\\n",(int)offsetof(struct TEST,mem));
  return 0;
}
""" % (header, t), filename)

    try:
      lines = self.run_static_program(filename)
    except ExecutionError:
      return None

    align = int(lines[0])
    print '%s: align: %s' % (typedef, align)
    return align


  def get_type_size_sign(self, typedef1, typedef2=None, header=''):
    if header:
      header = '#include <%s>' % header

    if typedef2 is None:
      typedef2 = typedef1
      expr1 = '(t1)0'
      expr2 = '(t1)~3 < ' + expr1
    else:
      expr1 = '(t1)0 + (t2)0'
      expr2 = '(t1)~3 + (t2)1 < ' + expr1

    filename = "tmp" + os.sep + "type_size_sign"
    self.write_src("""
#include <stddef.h>
#include <stdio.h>
%s

typedef %s t1;
typedef %s t2;

int main(int argc, char** argv) {
  printf("%%d\\n",(int)sizeof(%s));
  printf("%%d\\n", %s);

  return 0;
}
""" % (header, typedef1, typedef2, expr1, expr2), filename)

    try:
      lines = self.run_static_program(filename)
    except ExecutionError:
      return None, None
    size = int(lines[0])
    sign = int(lines[1])
    if typedef1 == typedef2:
      print '(%s)0: sign: %s' % (typedef1, sign)
    else:
      print '(%s)0 + (%s)0: sign: %s' % (typedef1, typedef2, sign)

    return size, sign

  ########

  native_int_types = [ \
      ('SCHAR', 'signed char'),
      ('UCHAR', 'unsigned char'),
      ('CHAR', 'char'),
      ('SHORT', 'short'),
      ('USHORT', 'unsigned short'),
      ('INT', 'int'),
      ('UINT', 'unsigned int'),
      ('LONG', 'long'),
      ('ULONG', 'unsigned long'),
      ('LONGLONG', 'long long'),
      ('ULONGLONG', 'unsigned long long'),
      ('BOOL', 'bool'),
      ]


  def detect_type_data(self, m, t, header=''):
    size = self.get_type_size(t, header)

    opt = self.options

    if size is None:
      if header:
        print "NO TYPE %s in <%s>" % (t, header)
      else:
        print "NO TYPE %s" % t

      opt.__dict__['HAVE_' + m] = False
    else:
      print '%s: size: %s' % (t, size)

      align = self.get_type_align(t, header)

      opt.__dict__['HAVE_' + m] = True
      opt.__dict__['SIZEOF_' + m] = size
      opt.__dict__['ALIGNOF_' + m] = align


  def detect_int_data(self):
    opt = self.options

    # find if we have stdint.h
    opt.HAVE_STDINT = self.check_header_exists("stdint.h")

    opt.sizesign2type = {}

    for m, t in self.native_int_types:
      self.detect_type_data(m, t)
      sizesign = self.get_type_size_sign(t)

      opt.sizesign2type[sizesign] = opt.sizesign2type.get(sizesign, t)


  def find_alias(self, t1, t2=None, header=''):
    size, sign = self.get_type_size_sign(t1, t2, header)
    if size is None:
      return None
    return self.options.sizesign2type[size, sign]


  def detect_aliases(self):
    opt = self.options
    opt.arith_conv = {}

    for m1, t1 in self.native_int_types:
      for m2, t2 in self.native_int_types:
        alias = self.find_alias(t1, t2)
        if alias is not None:
          opt.arith_conv[(t1, t2)] = alias

    for t in ['ptrdiff_t', 'size_t']:
      alias = self.find_alias(t)
      if alias:
        opt.__dict__['ALIAS_' + t] = alias

    if opt.HAVE_STDINT:
      for t in [
        'int8_t', 'uint8_t',
        'int16_t', 'uint16_t',
        'int32_t', 'uint32_t',
        'int64_t', 'uint64_t',
        'intmax_t', 'uintmax_t',
        'intptr_t', 'uintptr_t',
        ]:
        alias = self.find_alias(t, header='stdint.h')
        if alias:
          opt.__dict__['ALIAS_' + t] = alias

    if not getattr(opt,"ALIAS_int8",None):
      opt.ALIAS_int8_t="signed char"

    if not getattr(opt,"ALIAS_uint8",None):
      opt.ALIAS_uint8_t="unsigned char"

    sizes = {
      opt.SIZEOF_SHORT*8 : "short",
      opt.SIZEOF_INT*8: "int",
      opt.SIZEOF_LONG*8 : "long",
    }

    if opt.HAVE_LONGLONG:
      sizes[opt.SIZEOF_LONGLONG*8]="long long"

    if not getattr(opt,"ALIAS_intmax_t",None):
      opt.ALIAS_intmax_t="long"
      opt.ALIAS_uintmax_t="unsigned long"
      if opt.HAVE_LONGLONG:
        opt.ALIAS_intmax_t="long long"
        opt.ALIAS_uintmax_t="unsigned long long"

    for size in [16,32,64]:
      if not getattr(opt,"ALIAS_int"+str(size)+"_t",None):
        try:
          t = sizes[size]
          opt.__dict__["ALIAS_int"+str(size)+"_t"]=t
          opt.__dict__["ALIAS_uint"+str(size)+"_t"]="unsigned " + t
        except KeyError:
          opt.__dict__["ALIAS_int"+str(size)+"_t"]="emul_int"+str(size)
          opt.__dict__["ALIAS_uint"+str(size)+"_t"]="emul_uint"+str(size)

    if not getattr(opt,"ALIAS_intptr_t",None):
      try:
        opt.ALIAS_intptr_t=sizes[opt.SIZEOF_VOIDP*8]
        opt.ALIAS_uintptr_t="unsigned "+sizes[opt.SIZEOF_VOIDP*8]
      except:
        print "NO INTEGER THE SIZE OF A VOID*!"
        sys.exit(1)

  def detect_c_type_data(self):
    self.detect_int_data()

    std_dtypes = [
      ('BOOL', 'bool', ''),
      ('FLOAT', 'float', ''),
      ('DOUBLE', 'double', ''),
      ('LONGDOUBLE', 'long double', ''),
      ('ENUM', 'typedef enum enum_t {tag} t;', ''),
      ('VOIDP', 'void *', ''),
      ('FUNCP', 'typedef void (*t)(void);', ''),
      ('CBOOL', '_Bool', ''),
      ('WCHAR', 'wchar_t', 'stddef.h'),
      ('PTRDIFF', 'ptrdiff_t', 'stddef.h'),
      ('INTPTR', 'intptr_t', 'stdint.h'),
      ('UINTPTR', 'uintptr_t', 'stdint.h'),
      ('INTMAX', 'intmax_t', 'stdint.h'),
      ('UINTMAX', 'uintmax_t', 'stdint.h'),
      ('SIZE', 'size_t', 'stddef.h'),
      ('COMPLEX', 'float _Complex', ''),
      ('DOUBLECOMPLEX', 'double _Complex', ''),
      ('LONGDOUBLECOMPLEX', 'long double _Complex', ''),
      ('IMAGINARY', 'float _Imaginary', ''),
      ('DOUBLEIMAGINARY', 'double _Imaginary', ''),
      ('LONGDOUBLEIMAGINARY', 'long double _Imaginary', ''),
      ('INT8', 'int8_t', 'stdint.h'),
      ('INT16', 'int16_t', 'stdint.h'),
      ('INT32', 'int32_t', 'stdint.h'),
      ('INT64', 'int64_t', 'stdint.h'),
      ('UINT8', 'uint8_t', 'stdint.h'),
      ('UINT16', 'uint16_t', 'stdint.h'),
      ('UINT32', 'uint32_t', 'stdint.h'),
      ('UINT64', 'uint64_t', 'stdint.h'),
    ]

    for m, t, f in std_dtypes:
      self.detect_type_data(m, t, header=f)

    self.detect_aliases()

    opt = self.options


  def detect_win32(self):
    return self.check_macro_defined("_WIN32")

  def detect_win64(self):
    return self.check_macro_defined("_WIN64")

  def detect_posix(self):
    if self.check_macro_defined("_WIN32"):
      return False
    else:
      return True

  def detect_cygwin(self):
    return self.check_macro_defined("__CYGWIN__")

  def detect_osx(self):
    return self.check_macro_defined("__APPLE__")

  def detect_osx_version(self):
    if not self.options.MACOSX:
      return None
    else:
      # query gestalt to determine os version
      try:
        from gestalt import gestalt
        import MacOS
      except ImportError:
        return None

      try:
        sysv = gestalt('sysv')
      except (RuntimeError, MacOS.Error):
        return None

      major = (sysv & 0xff00) >> 8
      minor = (sysv & 0x00f0) >> 4
      patch = (sysv & 0x000f)

      # convert into decimal
      major = int(hex(major)[2:])

      version = major*100 + minor*10 + patch

      print 'MACOSX VERSION:', version

      return version

  def detect_bsd(self):
    return self.check_macro_defined("BSD")

  def detect_solaris(self):
    # can't find a symbol for solaris...
    # return self.check_macro_defined("__SOLARIS__")
    try:
      # print SunOS on solaris
      return self.shell("uname", verbose=False) == 'SunOS\n'
    except ExecutionError:
      return False

  def detect_linux(self):
    return self.check_macro_defined("__linux__")

  # only allows 4 models: cygwin, osx, win32 and posix
  # nocygwin = mingw = win32
  def detect_model(self):
    opt = self.options

    # check that we can use the compiler
    self.compile_dummy_main()

    opt.MACOSX = self.detect_osx()
    opt.MACOSX_VERSION = self.detect_osx_version()
    opt.BSD = self.detect_bsd()
    opt.SOLARIS = self.detect_solaris()
    opt.LINUX= self.detect_linux()
    opt.WIN32 = self.detect_win32()
    opt.WIN64 = self.detect_win64()
    opt.CYGWIN = self.detect_cygwin()
    opt.POSIX = self.detect_posix()

    sum = opt.MACOSX + opt.WIN32 + opt.CYGWIN
    if sum > 1:
      print "INCOMPATIBLE MODELS DETECTED"
      print "MACOSX",opt.MACOSX
      print "CYGWIN",opt.CYGWIN
      print "WIN32",opt.WIN32
    else:
     if opt.model == "detect":
       if opt.CYGWIN: opt.model = "cygwin"
       if opt.MACOSX: opt.model = "osx"
       if opt.WIN32: opt.model = "win32"
       if opt.WIN64: opt.model = "win64"
       if opt.SOLARIS: opt.model = "solaris"
       if opt.LINUX: opt.model = "linux"
       if opt.BSD: opt.model = "bsd"
       if opt.model == "detect": opt.model = "posix"
    if opt.model in ["mingw", "nocygwin"]: opt.model = "win32"
    if opt.model in ["posix"] and opt.CYGWIN: opt.model = "cygwin"

    if opt.model not in ["posix", "linux", "solaris", "bsd", "osx", "cygwin", "win32", "win64"]:
      print "UNKNOWN MODEL", opt.model
      sys.exit(1)

    print "MODEL=", self.options.model

  ########

  def detect_intsizes(self):
    """find misc info about endianess"""

    opt = self.options

    filename = "tmp" + os.sep + "intsizes"

    try:
      output = self.run_static_string_program(r"""
#include <stdio.h>
#include <stddef.h>

enum enum_t {e_tag};
typedef void (*fp_t)(void);

union endian_t {
  unsigned long x;
  unsigned char y[sizeof(unsigned long)];
} endian;

int main(int argc, char** argv) {
  printf("CHAR_IS_UNSIGNED=%d\n",((char)0xFF)>0?1:0);
  endian.x = 1ul;
  printf("LITTLE_ENDIAN=%d\n", endian.y[0]);
  printf("BIG_ENDIAN=%d\n", endian.y[sizeof(unsigned long)-1]);

  return 0;
}
""", filename)
    except ExecutionError:
      print "FATAL: can't determine sizes of ints"
      raise
    ## THIS CALL CANNOT USE THE SHELL BECAUSE IT REDIRECTS OUTPUT
    os.system("tmp"+os.sep+"intsizes > tmp"+os.sep+"intsizes.py")
    f = open("tmp"+os.sep+"intsizes.py")
    try:
      exec f in opt.__dict__
    finally:
      f.close()
    # RF (zzz): getting \r\n output from target/xcompile stage,
    # under nocygwin which is confusing 'exec' which flags them
    # as syntax errors. re-enabling the old code (not sure why it was
    # changed as it sure as hell weren't broke).

    # exec string.join(output, '\n') in opt.__dict__

    if opt.CHAR_IS_UNSIGNED:
      print "char is unsigned"
    else:
      print "char is signed"

    if opt.BIG_ENDIAN: print "Big Endian byte order detected"
    if opt.LITTLE_ENDIAN: print "Little Endian byte order detected"


  def detect_alignment(self):
    #calculate alignment tables
    vbls = [
      ("ALIGNOF_CBOOL","_Bool"),
      ("ALIGNOF_BOOL","bool"),
      ("ALIGNOF_SHORT","short"),
      ("ALIGNOF_INT","int"),
      ("ALIGNOF_LONG","long"),
      ("ALIGNOF_LONGLONG","long long"),

      ("ALIGNOF_FLOAT","float"),
      ("ALIGNOF_DOUBLE","double"),
      ("ALIGNOF_LONGDOUBLE","long double"),

      ("ALIGNOF_WCHAR","wchar_t"),
      ("ALIGNOF_VOIDP","void*"),
      ]

    opt = self.options

    opt.MAX_ALIGN = 1
    opt.flx_aligns = {}
    for k, t in vbls:
      try:
        v = opt.__dict__[k]
        opt.flx_aligns[v]=t
      except KeyError:
        pass
      else:
        if v > opt.MAX_ALIGN:
          opt.MAX_ALIGN = v
    opt.flx_aligns[1] = "char"


  def detect_isnan(self):
    # find if we have C99 isnan in <math.h>
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isnan(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_C99_ISNAN_IN_MATH = True
      print "C99 isnan found in <math.h>"
    except ExecutionError:
      self.options.HAVE_C99_ISNAN_IN_MATH = False

    # find if we have BSD isnanf in <math.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isnanf(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_BSD_ISNAN_IN_MATH = True
      print "Isnan found in <math.h>"
    except ExecutionError:
      self.options.HAVE_BSD_ISNAN_IN_MATH = False

    # find if we have BSD isnanf in <ieeefp.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <ieeefp.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isnanf(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_ieeefp')
      self.options.HAVE_BSD_ISNAN_IN_IEEEFP = True
      print "Isnan found in <ieeefp.h>"
    except ExecutionError:
      self.options.HAVE_BSD_ISNAN_IN_IEEEFP = False

    # find if we have C99 isinf in <math.h>
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isinf(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_C99_ISINF_IN_MATH = True
      print "C99 isnan found in <math.h>"
    except ExecutionError:
      self.options.HAVE_C99_ISINF_IN_MATH = False

    # find if we have BSD isinff in <math.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isinff(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_BSD_ISINF_IN_MATH = True
      print "BSD isinf found in <math.h>"
    except ExecutionError:
      self.options.HAVE_BSD_ISINF_IN_MATH = False

    # find if we have BSD isinff in <ieeefp.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <ieeefp.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isinff(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_ieeefp')
      self.options.HAVE_BSD_ISINF_IN_IEEEFP = True
      print "BSD isinf found in <ieeefp.h>"
    except ExecutionError:
      self.options.HAVE_BSD_ISINF_IN_IEEEFP = False

    # find if we have C99 isfinite in <math.h>
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  isfinite(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_C99_ISFINITE_IN_MATH = True
      print "C99 isfinite found in <math.h>"
    except ExecutionError:
      self.options.HAVE_C99_ISFINITE_IN_MATH = False

    # find if we have BSD finitef in <math.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <math.h>

int main(int argc,char** argv) {
  float f = 0.0;
  finitef(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_math')
      self.options.HAVE_BSD_FINITE_IN_MATH = True
      print "BSD finitef found in <math.h>"
    except ExecutionError:
      self.options.HAVE_BSD_FINITE_IN_MATH = False

    # find if we have BSD isfinitef in <ieeefp.h> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <ieeefp.h>

int main(int argc,char** argv) {
  float f = 0.0;
  finitef(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_ieeefp')
      self.options.HAVE_BSD_FINITE_IN_IEEEFP = True
      print "BSD finitef found in <ieeefp.h>"
    except ExecutionError:
      self.options.HAVE_BSD_FINITE_IN_IEEEFP = False


  def detect_vsnprintf(self):
    opt = self.options

    filename = self.write_src(r"""
#include <stdio.h>
#include <stdarg.h>

int check(char const*fmt,...)
{
  va_list ap;
  va_start(ap,fmt);
  int n = vsnprintf(NULL,0,fmt,ap);
  va_end(ap);
  return n!=3;
}

int main(int argc,char** argv) {
  return check("%s","XXX"); // 0 means pass
}
""", 'tmp' + os.sep + 'vsnprintf')

    try:
      lines = self.run_static_program(filename)
    except ExecutionError:
      opt.HAVE_VSNPRINTF = False
    else:
      opt.HAVE_VSNPRINTF = True

    if opt.HAVE_VSNPRINTF:
      print "vsnprintf() supported"
    else:
      print "vsnprintf() NOT supported"

  ########

  def detect_compiler_options(self):
    pass

  def check_options(self):
    self.detect_model()

    self.detect_compiler_options()

    self.detect_intsizes()
    self.detect_c_type_data()
    self.detect_alignment()
    self.detect_isnan()
    self.detect_vsnprintf()

    # would like to know if we have SDL_opengl. that's done by compiling
    # and running sdl_opengl.cxx Needs can be compiled like
    # g++ `sdl-config --cflags` sdl_opengl.cxx `sdl-config --libs`
    # but how is that done portably? Does win32 even have sdl-config?


  def report_config(self):
    opt = self.options
    print "**********************************************"
    print opt.COM, opt.use, "configuration"
    print "**********************************************"
    print "model=", opt.model
    print "static library tool #1                  :",  opt.AR
    print "static library tool #2                  :",  opt.RANLIB
    print
    print "Command to compile static Felix rtl     :",  opt.CCOBJ_STATIC_RTL
    print "Command to compile shared Felix rtl     :",  opt.CCOBJ_DYNAMIC_RTL
    print "Command to link shared Felix rtl        :",  opt.CCLINK_DYNAMIC_RTL
    print
    print "Command to compile static Felix driver  :",  opt.CCOBJ_STATIC_MAIN
    print "Command to compile dynamic Felix driver :",  opt.CCOBJ_DYNAMIC_MAIN
    print "Command to link dynamic Felix driver    :",  opt.CCLINK_DYNAMIC_MAIN
    print
    print "Command to compile static Felix object  :",  opt.CCOBJ_STATIC_FLX
    print "Command to compile loadable Felix object:",  opt.CCOBJ_DYNAMIC_FLX
    print "Command to link loadable Felix object   :",  opt.CCLINK_DYNAMIC_FLX
    print
    print "Extension for static object file        :",  opt.EXT_STATIC_OBJ
    print "Extension for shared object file        :",  opt.EXT_SHARED_OBJ
    print "Extension for static archive            :",  opt.EXT_LIB
    print "Extension for loadable RTL              :",  opt.EXT_DYLIB
    print "Extension for flx modules               :",  opt.EXT_SHLIB
    print "Extension for executable                :",  opt.EXT_EXE
    print "RTL in directory                        :",  opt.SHLIB_DIR

    print
    self.report_isnan()

#!/usr/bin/env python

import os
import sys
import time
import glob
import getopt
import shutil

sys.path.append(
  os.path.join(os.path.dirname(sys.argv[0]), '..', 'lib')
)

os.environ['PYTHONPATH'] = os.pathsep.join([
  os.path.join(os.path.dirname(sys.argv[0]), '..', 'lib'),
  os.environ.get('PYTHONPATH', ''),
])

from fbuild.flxbuild.flxutil import xqt, ExecutionError
from fbuild.flxbuild.config_support import pr, pa

from version import *

#----------------------------------------

print 'flx_version', flx_version
print 'flx_version_major', flx_version_major
print 'godi_revision', godi_revision
print 'debian_revision', debian_revision

time_stamp_format = "%Y/%m/%d %H:%M:%S UTC"
config_time = time.gmtime(time.time())
CONFIG_TIME = time.strftime(time_stamp_format, config_time)

#----------------------------------------

verbose = 1
quiet = 1
force = 0
upgrade = 0
refresh = 0
default_prefix='/usr/local'

PREFIX=os.environ.get("PREFIX",default_prefix)
if PREFIX:
  print "Default Installation Root from environment: ",PREFIX

overrides = {}

# supported platforms

platforms = [
  "posix",
  "cygwin",
  "nocygwin",
  "mingw",
  "win32",
  "win64",
  "osx",
  "detect"
  "solaris",
  "bsd",
  "linux"
  ]

# map other names for them, obtained from
# various place like os.platform, os.system("mname -u"), etc

archmap = {
  "irix":"posix",
  "irix64":"posix",
  "unix":"posix",
  "posix":"posix",
  "linux":"linux",
  "gnu/linux":"linux",
  "solaris":"solaris",
  "sunos":"solaris",
  "cygwin":"cygwin",
  "nocygwin":"nocygwin",
  "mingw":"mingw",
  "windows":"win32",
  "nt":"win32",
  "win32":"win32",
  "win64":"win64",
  "darwin":"osx",
  "freebsd":"bsd",
  "netbsd":"bsd",
  "openbsd":"bsd",
  "osx":"osx",
  "detect":"detect"
  }

# attempt to find the Felix name for the build OS
# using uname -s, or, if that fails, Python os.name
# if the final result isn't a name we recognize
# set the build_model to 'detect' to indicate C level
# testing is to be used. Note that these C tests are
# done anyhow, and may verify, refine, or otherwise
# munge this result .. however we need some initial
# indication HOW to perform these tests.

try:
  output = xqt('uname', '-s')
except ExecutionError:
  try:
    build_model = archmap[os.name]
  except KeyError:
    print "uname -s and Python returns unknown OS type, assuming 'detect'"
    build_model = "detect"
else:
  output = output[0].strip().lower()
  build_model = archmap[output]

print "Build platform: " + build_model

# RF: noone seems to be using the results of this
# JS: Not yet: policy is to test it out anyhow, in case needed
#
# uname -s: kernel name "linux" on linux
# uname -n: network node name "rosella" on JS box
# uname -r: kernel-release "2.6.12-10-amd64-k8" on JS box
# uname -v: kernel-version " #1 Thu Dec 22 11:12:06 UTC 2005" on JS box
# uname -m: machine hardware name: "x86_64" on JS box
# uname -o: operating system: "GNU/Linux" on JS box
# uname -p: OSX only? on osx reports broad cpu type (e.g. powerpc)
# not sure what it reports on intel macs.
# machine command reports very specific cpu type, e.g. ppc7450, ppc7400

try:
  output = xqt('uname', '-m')
except ExecutionError:
  ARCH = "unknown"
else:
  ARCH = output[0].strip().lower()

print "CPU=",ARCH

try:
  if build_model == 'osx':
    output = xqt('uname -p')
  else:
    output = xqt('uname -o')
except ExecutionError:
  OS = 'unknown'
else:
  OS = output[0].strip().lower()

print "OS=",OS


print "Detected Build model:",build_model

host_model = None
target_model = None
run_model = None

def check_model(m):
  try:
    m = archmap[m]
  except KeyError:
    print "Unknown model '"+m+"' please choose one of:"
    for m in platforms: print " * " + m
    sys.exit(1)
  return m

SAVE_CONFIG=""
LOAD_CONFIG=""
BOOTFILE=""

include_paths=[]
lib_paths=[]

BUILDCC=None
HOSTCC=None
TARGETCC=None

BUILDCXX=None
HOSTCXX=None
TARGETCXX=None

src_dir=os.environ.get("SRC_DIR",os.curdir)
FLX_LPARCHIVE=os.environ.get("FLX_LPARCHIVE",os.curdir)
FLX_RTL_DIR=os.path.join('lib', 'rtl')
FLX_TARGET_CONFIG_DIR=os.path.join('config', 'target')
FLX_HOST_CONFIG_DIR=os.path.join('config', 'host')

ALL_PHASES=["build","host","target","run"]

PHASES = []

try:
  opts, args = getopt.getopt(sys.argv[1:], 'hvq',
      ['help', 'verbose', 'quiet', 'force', 'refresh', 'upgrade',
      'prefix=', 'set-int=', 'set-string=',
      'include_paths=', 'lib_paths=',
      'build=', 'host=', 'target=', 'run=',
      'buildcc=', 'hostcc=', 'targetcc=', 'targetcxx=',
      'boot=', 'save-config=',
      'load-config=', 'lparchive=', 'phase='])
      #'set-int',
except getopt.error, e:
  sys.stderr.write(str(e) + '\n')
  sys.exit(1)

for o, a in opts:
  if o in ['-h', '--help']:
    print """\
usage: configure [options]

flags:
  -h, --help      print this help message
  -v, --verbose   print out extra debugging info
  -q, --quiet     do not print out extra debugging info

  --force
  --refresh
  --upgrade
  --prefix        install in this prefixed directory
  --set-int
  --set-string
  --include_paths additionally search these paths for headers
  --lib_paths     additionally search these paths for libraries
  --build
  --host
  --target
  --run
  --buildcc
  --hostcc
  --targetcc
  --buildcxx
  --hostcxx
  --targetcxx
  --boot
  --save-config
  --load-config
  --lparchive
  --phase
"""
    sys.exit(0)
  elif o in ['-v', '--verbose']:
    verbose = verbose + 1
    quiet = 0
  elif o in ['-q', '--quiet']:
    verbose = 0
  elif o in ['--force']:
    force = 1
  elif o in ['--refresh']:
    refresh = 1
  elif o in ['--upgrade']:
    upgrade = 1
  elif o in ['--prefix']:
    PREFIX = a
  elif o in ['--set-int']:
    v, a = a.split('=', 1)
    overrides[v] = int(a)
  elif o in ['--set-string']:
    v, a = a.split('=', 1)
    overrides[v] = a
  elif o in ['--include_paths']:
    include_paths.append(a)
  elif o in ['--lib_paths']:
    lib_paths.append(a)
  elif o in ['--build']:
    print "Specified build model", a
    build_model = check_model(a)
  elif o in ['--host']:
    print "Specified host model:", a
    host_model = check_model(a)
  elif o in ['--target']:
    print "Specified target model:", a
    target_model = check_model(a)
  elif o in ['--run']:
    print "Specified run model", a
    run_model = check_model(a)
  elif o in ['--buildcc']:
    BUILDCC = a
  elif o in ['--hostcc']:
    HOSTCC = a
  elif o in ['--targetcc']:
    TARGETCC = a
  elif o in ['--buildcxx']:
    BUILDCXX = a
  elif o in ['--hostcxx']:
    HOSTCXX = a
  elif o in ['--targetcxx']:
    TARGETCXX = a
  elif o in ['--boot']:
    BOOTFILE = a
  elif o in ['--save-config']:
    SAVE_CONFIG = a
  elif o in ['--load-config']:
    LOAD_CONFIG = a
  elif o in ['--lparchive']:
    FLX_LPARCHIVE = a
  elif o in ['--phase']:
    if a not in ALL_PHASES:
      print "UNKNOWN PHASE",a,"not in",ALL_PHASES
      sys.exit(1)
    if a not in PHASES: PHASES.append(a)
  else:
    print "UNKNOWN CONFIGURE OPTION", o, a
    sys.exit(1)

if args:
  print "UNKNOWN CONFIGURE ARGS", args
  sys.exit(1)

if not verbose:
  quiet = 2

if PHASES == []: PHASES = ALL_PHASES

if BOOTFILE:
  try:
    execfile(BOOTFILE)
    print "Loaded",BOOTFILE
  except:
    print "Cannot execute specified bootstrap file: ", BOOTFILE
    sys.exit(1)

if PREFIX=='': PREFIX=default_prefix
print "INSTALL PREFIX="+PREFIX
if src_dir=='': src_dir = os.curdir
if FLX_LPARCHIVE=='': FLX_LPARCHIVE = os.curdir
print "src_dir="+src_dir
print "FLX_LPARCHIVE="+FLX_LPARCHIVE
print "FLX_RTL_DIR="+FLX_RTL_DIR
print "FLX_TARGET_CONFIG_DIR="+FLX_TARGET_CONFIG_DIR
print "FLX_HOST_CONFIG_DIR="+FLX_HOST_CONFIG_DIR

#---------------------------------------------------
# Discover C/C++ compilers, linker, and other 'binutils'

import fbuild.flxbuild.ocaml_class
import fbuild.flxbuild.gcc_class
import fbuild.flxbuild.gxx_class
import fbuild.flxbuild.msvcc_class
import fbuild.flxbuild.msvcxx_class

#
# Detect the native build model
#
# This model has two uses: first, to build any build time
# tools needed to assist in generating the sources
# and second, to aid in selecting the options to cross-compile
# for the chosen host and target
#

if not os.path.exists('config'):
  os.path.mkdir('config')

this = globals()

if "build" in PHASES:
  print
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print "Checking BUILD MODEL",build_model
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print

  if build_model in ["win32","win64"]:
    BUILD_CC = fbuild.flxbuild.msvcc_class.msvcc(verbose=verbose, quiet=quiet)
    BUILD_CXX = fbuild.flxbuild.msvcxx_class.msvcxx(verbose=verbose, quiet=quiet)
  else:
    BUILD_CC = fbuild.flxbuild.gcc_class.gcc(verbose=verbose, quiet=quiet)
    BUILD_CXX = fbuild.flxbuild.gxx_class.gxx(verbose=verbose, quiet=quiet)

  BUILD_CC.set_options(
      COM=BUILDCC,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="build",
      model=build_model,
      build=build_model)

  BUILD_CC.check_options()
  BUILD_CC.report_config()
  BUILD_CC.save_options("config/build_cc.py")
  build_model=BUILD_CC.options.model

  BUILD_CXX.set_options(
      COM=BUILDCXX,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="build",
      model=build_model,
      build=build_model)

  BUILD_CXX.check_options()
  BUILD_CXX.report_config()
  BUILD_CXX.save_options("config/build_cxx.py")
  build_model=BUILD_CXX.options.model

  try:
    print "Writing build config file"
    f = open("config"+os.sep+"build_config.py","w")
    pr(f,'import sys')
    pr(f,"if '' not in sys.path: sys.path = [''] + sys.path")
    print "WRITE STEP 0"
    pr(f,'#'+CONFIG_TIME)
    print "WRITE STEP 1"
    pa(f,this,"CONFIG_TIME")
    pa(f,this,"flx_version")
    pa(f,this,"flx_version_major")
    pa(f,this,"godi_revision")
    pa(f,this,"debian_revision")
    pr(f,"import fbuild.flxbuild.gcc_class")
    pr(f,"import fbuild.flxbuild.gxx_class")
    pr(f,"import fbuild.flxbuild.msvcc_class")
    pr(f,"import fbuild.flxbuild.msvcxx_class")
    pr(f,"from fbuild.flxbuild.config_support import *")

    pa(f,this,"build_model")
    pa(f,this,"src_dir")
    pa(f,this,"FLX_LPARCHIVE")
    pa(f,this,"FLX_RTL_DIR")
    pa(f,this,"FLX_TARGET_CONFIG_DIR")
    pa(f,this,"FLX_HOST_CONFIG_DIR")
    pr(f,"")

    print "WRITE STEP 2"

    cc = BUILD_CC.__class__.__name__
    pr(f,"BUILD_CC = fbuild.flxbuild."+cc+"_class."+cc+"()")
    pr(f,"BUILD_CC.load_options("+repr('config'+os.sep+'build_cc.py')+")")

    cxx = BUILD_CXX.__class__.__name__
    pr(f,"BUILD_CXX = fbuild.flxbuild."+cxx+"_class."+cxx+"()")
    pr(f,"BUILD_CXX.load_options("+repr('config'+os.sep+'build_cxx.py')+")")
    f.close()
  except EnvironmentError:
    print "Unable to create config"+os.sep+"build_config.py"
    sys.exit(1)

  print "Created config"+os.sep+"build_config.py"
  print "Edit this file to set your preferences"
  print "This file will not be clobbered by the Felix build process"

  cpkgs =  glob.glob("cpkgs"+os.sep+"build"+os.sep+"*.py")
  for cpkgf in cpkgs:
    cpkg = os.path.splitext(os.path.basename(cpkgf))[0]
    print "build CONFIGURING", cpkg
    __import__('cpkgs.build.' + cpkg)


if "host" in PHASES:
  #
  # Now create the host model: the compiler has to run
  # on the build machine, but can cross compile for
  # the host (if so, we can build but not test Felix)
  #
  # Cross compilation of the host tools may prevent any
  # testing of the tools
  #


  if not host_model:
    host_model=build_model
    print "Defaulting host model to build model",host_model
  if not target_model:
    target_model = host_model
    print "Defaulting target model to host model:",target_model


  print
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print "Checking HOST MODEL",host_model
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print

  if host_model in ["win32","win64"]:
    HOST_CC = fbuild.flxbuild.msvcc_class.msvcc(verbose=verbose, quiet=quiet)
    HOST_CXX = fbuild.flxbuild.msvcxx_class.msvcxx(verbose=verbose, quiet=quiet)
  else:
    HOST_CC = fbuild.flxbuild.gcc_class.gcc(verbose=verbose, quiet=quiet)
    HOST_CXX = fbuild.flxbuild.gxx_class.gxx(verbose=verbose, quiet=quiet)

  HOST_CC.set_options(
      COM=HOSTCC,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="host",
      model=host_model,
      build=build_model)

  # check if we can just copy the build compiler
  if \
      HOST_CC.options.COM == BUILD_CC.options.COM and \
      HOST_CC.options.model == BUILD_CC.options.model and \
      HOST_CC.options.build == BUILD_CC.options.build:
    print 'using the BUILD_CC options for the HOST_CC'
    HOST_CC.load_options("config"+os.sep+"build_cc.py")
    HOST_CC.options.use = "host"
  else:
    HOST_CC.check_options()
    HOST_CC.report_config()
  HOST_CC.save_options("config"+os.sep+"host_cc.py")

  HOST_CXX.set_options(
      COM=HOSTCXX,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="host",
      model=host_model,
      build=build_model)

  # check if we can just copy the build compiler
  if \
      HOST_CXX.options.COM == BUILD_CXX.options.COM and \
      HOST_CXX.options.model == BUILD_CXX.options.model and \
      HOST_CXX.options.build == BUILD_CXX.options.build:
    print 'using the BUILD_CXX options for the HOST_CXX'
    HOST_CXX.load_options("config"+os.sep+"build_cxx.py")
    HOST_CXX.options.use = "host"
  else:
    HOST_CXX.check_options()
    HOST_CXX.report_config()
  HOST_CXX.save_options("config"+os.sep+"host_cxx.py")

  HOST_OCAML = fbuild.flxbuild.ocaml_class.ocaml(verbose=verbose, quiet=quiet)

  camllinkopts = ""
  if HOST_CXX.options.CYGWIN:
    camllinkopts = '-ccopt "-Wl,--stack -Wl,10485760" '

  HOST_OCAML.autodetect(camllinkopts)
  HOST_OCAML.report_config()
  HOST_OCAML.save_options("config/ocaml_config.py")


  host_model = HOST_CXX.options.model
  try:
    print "Writing host config file"
    f = open("config"+os.sep+"host_config.py","w")
    pr(f,'import sys')
    pr(f,"if '' not in sys.path: sys.path = [''] + sys.path")
    pr(f,'#'+CONFIG_TIME)
    pa(f,this,"CONFIG_TIME")
    pa(f,this,"flx_version")
    pa(f,this,"flx_version_major")
    pa(f,this,"godi_revision")
    pa(f,this,"debian_revision")
    pr(f,"import fbuild.flxbuild.gcc_class")
    pr(f,"import fbuild.flxbuild.msvcc_class")
    pr(f,"import fbuild.flxbuild.gxx_class")
    pr(f,"import fbuild.flxbuild.msvcxx_class")
    pr(f,"import fbuild.flxbuild.ocaml_class")
    pr(f,"from fbuild.flxbuild.config_support import *")

    pa(f,this,"build_model")
    pa(f,this,"host_model")
    pa(f,this,"src_dir")
    pa(f,this,"FLX_LPARCHIVE")
    pa(f,this,"FLX_RTL_DIR")
    pa(f,this,"FLX_HOST_CONFIG_DIR")
    pr(f,"")

    pr(f,"HOST_OCAML = fbuild.flxbuild.ocaml_class.ocaml()")
    pr(f,"HOST_OCAML.load_options("+repr('config'+os.sep+'ocaml_config.py')+")")

    cc = HOST_CC.__class__.__name__
    pr(f,"HOST_CC = fbuild.flxbuild."+cc+"_class."+cc+"()")
    pr(f,"HOST_CC.load_options("+repr('config'+os.sep+'host_cc.py')+")")

    cxx = HOST_CXX.__class__.__name__
    pr(f,"HOST_CXX = fbuild.flxbuild."+cxx+"_class."+cxx+"()")
    pr(f,"HOST_CXX.load_options("+repr('config'+os.sep+'host_cxx.py')+")")
    f.close()
  except EnvironmentError:
    print "Unable to create config"+os.sep+"host_config.py"
    sys.exit(1)

  print "Created config"+os.sep+"host_config.py"
  print "Edit this file to set your preferences"
  print "This file will not be clobbered by the Felix build process"

  cpkgs =  glob.glob("cpkgs"+os.sep+"host"+os.sep+"*.py")
  for cpkgf in cpkgs:
    cpkg = os.path.splitext(os.path.basename(cpkgf))[0]
    print "host CONFIGURING", cpkg
    __import__('cpkgs.host.' + cpkg)


if "target" in PHASES:
  #
  # Now create the target model: the compiler has to run
  # on the build machine, but can cross compile for
  # the target
  #
  # cross compilation of C++ generated by Felix allows us to
  # check the generated code compiles, but not that it runs
  # [but the output is largely portable so we can still try]
  #

  print
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print "Checking TARGET MODEL",target_model
  print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  print

  if target_model in ["win32","win64"]:
    TARGET_CC = fbuild.flxbuild.msvcc_class.msvcc(verbose=verbose, quiet=quiet)
    TARGET_CXX = fbuild.flxbuild.msvcxx_class.msvcxx(verbose=verbose, quiet=quiet)
  else:
    TARGET_CC = fbuild.flxbuild.gcc_class.gcc(verbose=verbose, quiet=quiet)
    TARGET_CXX = fbuild.flxbuild.gxx_class.gxx(verbose=verbose, quiet=quiet)

  TARGET_CC.set_options(
      COM=TARGETCC,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="target",
      model=target_model,
      build=build_model)

  # check if we can just copy the build compiler
  if \
      TARGET_CC.options.COM == BUILD_CC.options.COM and \
      TARGET_CC.options.model == BUILD_CC.options.model and \
      TARGET_CC.options.build == BUILD_CC.options.build:
    print 'using the BUILD_CC options for the TARGET_CC'
    TARGET_CC.load_options("config"+os.sep+"build_cc.py")
    TARGET_CC.options.use = "target"
  elif \
      TARGET_CC.options.COM == HOST_CC.options.COM and \
      TARGET_CC.options.model == HOST_CC.options.model and \
      TARGET_CC.options.build == HOST_CC.options.build:
    print 'using the HOST_CC options for the TARGET_CC'
    TARGET_CC.load_options("config"+os.sep+"host_cc.py")
  else:
    TARGET_CC.check_options()
    TARGET_CC.report_config()
  TARGET_CC.save_options("config"+os.sep+"target_cc.py")

  TARGET_CXX.set_options(
      COM=TARGETCXX,
      include_paths=include_paths,
      lib_paths=lib_paths,
      use="target",
      model=target_model,
      build=build_model)

  # check if we can just copy the build compiler
  if \
      TARGET_CXX.options.COM == BUILD_CXX.options.COM and \
      TARGET_CXX.options.model == BUILD_CXX.options.model and \
      TARGET_CXX.options.build == BUILD_CXX.options.build:
    print 'using the BUILD_CXX options for the TARGET_CXX'
    TARGET_CXX.load_options("config"+os.sep+"build_cxx.py")
    TARGET_CXX.options.use = "target"
  elif \
      TARGET_CXX.options.COM == HOST_CXX.options.COM and \
      TARGET_CXX.options.model == HOST_CXX.options.model and \
      TARGET_CXX.options.build == HOST_CXX.options.build:
    print 'using the HOST_CXX options for the TARGET_CXX'
    TARGET_CXX.load_options("config"+os.sep+"host_cxx.py")
    TARGET_CXX.options.use = "target"

  else:
    TARGET_CXX.check_options()
    TARGET_CXX.report_config()
  TARGET_CXX.save_options("config"+os.sep+"target_cxx.py")

  if target_model in ["win32","win64"]:
    HAVE_MSVC = 1
    HAVE_GNU = 0
    DIFF = 'FC /L /W'
  else:
    HAVE_MSVC = 0
    HAVE_GNU = 1

    #DIFF = "diff -a -b " # build system is Unix Python
    # RF - trying out args that work on solaris (-a = not cool)
    # could use that sys type stuff here?
    DIFF = 'diff -b'

  ISCR = sys.executable + ' ' + \
    os.path.join(src_dir, 'interscript', 'bin', 'iscr.py') + \
    ' --cache-prefix=lpsrc-cache'

  # target model switches
  CYGWIN = TARGET_CXX.options.CYGWIN
  MACOSX = TARGET_CXX.options.MACOSX
  WIN32 =  TARGET_CXX.options.WIN32
  WIN64 =  TARGET_CXX.options.WIN32
  LINUX =  TARGET_CXX.options.LINUX
  SOLARIS =  TARGET_CXX.options.SOLARIS
  POSIX =  TARGET_CXX.options.POSIX
  BSD =  TARGET_CXX.options.BSD

  target_model = TARGET_CXX.options.model
  if not run_model:
    run_model = target_model
    print "Defaulting run model to target model:",run_model

  SUPPORT_DYNAMIC_LOADING = TARGET_CXX.options.SUPPORT_DYNAMIC_LOADING

  if SUPPORT_DYNAMIC_LOADING:
    DEFAULT_LINK_MODEL="dynamic"
  else:
    DEFAULT_LINK_MODEL="static"

  try:
    print "Writing main config file"
    f = open("config"+os.sep+"__init__.py","w")
    pr(f,'import sys')
    pr(f,"if '' not in sys.path: sys.path = [''] + sys.path")
    pr(f,'#'+CONFIG_TIME)
    pa(f,this,"CONFIG_TIME")
    pa(f,this,"flx_version")
    pa(f,this,"flx_version_major")
    pa(f,this,"godi_revision")
    pa(f,this,"debian_revision")
    if BOOTFILE:
      pr(f,"try:")
      pr(f,"  execfile('config/config_bootstrap.py')")
      pr(f,"except: pass")
    pr(f,"import fbuild.flxbuild.gcc_class")
    pr(f,"import fbuild.flxbuild.msvcc_class")
    pr(f,"import fbuild.flxbuild.gxx_class")
    pr(f,"import fbuild.flxbuild.msvcxx_class")
    pr(f,"import fbuild.flxbuild.ocaml_class")
    pr(f,"from fbuild.flxbuild.config_support import *")

    pr(f,"")
    pr(f,"#User configurable section")
    pa(f,this,"SUPPORT_DYNAMIC_LOADING")
    pr(f,"SUPPORT_STATIC_LINKAGE = 1")
    pa(f,this,"DEFAULT_LINK_MODEL")
    pa(f,this,"build_model")
    pa(f,this,"host_model")
    pa(f,this,"target_model")
    pa(f,this,"run_model")
    pa(f,this,"CYGWIN")
    pa(f,this,"MACOSX")
    pa(f,this,"WIN32")
    pa(f,this,"WIN64")
    pa(f,this,"POSIX")
    pa(f,this,"SOLARIS")
    pa(f,this,"BSD")
    pa(f,this,"LINUX")
    pa(f,this,"PREFIX")
    pa(f,this,"src_dir")
    pa(f,this,"FLX_LPARCHIVE")
    pa(f,this,"FLX_RTL_DIR")
    pa(f,this,"FLX_HOST_CONFIG_DIR")
    pa(f,this,"FLX_TARGET_CONFIG_DIR")
    pr(f,"")

    pr(f,"HOST_OCAML = fbuild.flxbuild.ocaml_class.ocaml()")
    pr(f,"HOST_OCAML.load_options("+repr('config'+os.sep+'ocaml_config.py')+")")

    cc = HOST_CC.__class__.__name__
    pr(f,"HOST_CC = fbuild.flxbuild."+cc+"_class."+cc+"()")
    pr(f,"HOST_CC.load_options("+repr('config'+os.sep+'host_cc.py')+")")
    cc = TARGET_CC.__class__.__name__
    pr(f,"TARGET_CC = fbuild.flxbuild."+cc+"_class."+cc+"()")
    pr(f,"TARGET_CC.load_options("+repr('config'+os.sep+'target_cc.py')+")")

    cxx = HOST_CXX.__class__.__name__
    pr(f,"HOST_CXX = fbuild.flxbuild."+cxx+"_class."+cxx+"()")
    pr(f,"HOST_CXX.load_options("+repr('config'+os.sep+'host_cxx.py')+")")
    cxx = TARGET_CXX.__class__.__name__
    pr(f,"TARGET_CXX = fbuild.flxbuild."+cxx+"_class."+cxx+"()")
    pr(f,"TARGET_CXX.load_options("+repr('config'+os.sep+'target_cxx.py')+")")
    pr(f,"")
    pa(f,this,"HAVE_GNU")
    pr(f,"FLXCC_CPP='cpp '")
    pa(f,this,"HAVE_MSVC")
    pa(f,this,"DIFF")
    pa(f,this,"ISCR")
    pr(f,"")
    pr(f,"# HACK to get all the target variables into global namespace")
    f.close()
    if BOOTFILE:
      print "Copying bootfile :  "+ BOOTFILE
      shutil.copy(BOOTFILE, os.path.join('config', 'config_bootstrap.py'))
  except EnvironmentError:
    print "Unable to create config"+os.sep+"__init__.py"
    sys.exit(1)

  print "Created config"+os.sep+"__init__.py"
  print "Edit this file to set your preferences"
  print "This file will not be clobbered by the Felix build process"

  cpkgs =  glob.glob("cpkgs"+os.sep+"target"+os.sep+"*.py")
  for cpkgf in cpkgs:
    cpkg = os.path.splitext(os.path.basename(cpkgf))[0]
    print "target CONFIGURING", cpkg
    __import__('cpkgs.target.' + cpkg)

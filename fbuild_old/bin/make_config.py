#!/usr/bin/env python

import os
import sys
import time
import glob
from optparse import OptionParser, make_option
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

import version

# -----------------------------------------------------------------------------

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

ALL_PHASES = ["build", "host", "target", "run"]

# -----------------------------------------------------------------------------

def check_model(m):
  try:
    m = archmap[m]
  except KeyError:
    print "Unknown model '"+m+"' please choose one of:"
    for m in platforms: print " * " + m
    sys.exit(1)
  return m

def load_options():
    parser = OptionParser()
    parser.add_options([
        make_option('-v', '--verbose',
            action='count',
            default=0,
            help='print out extra debugging info'),
        make_option('-q', '--quiet',
            dest='verbose',
            action='store_const',
            const=0,
            help='do not print out extra debugging info'),
        make_option('--prefix',
            help='install into this prefixed directory',
            default=os.environ.get('PREFIX', '/usr/local')),
        make_option('-I', '--include-path',
            metavar='INCLUDE_PATH',
            dest='include_paths',
            action='append',
            help='additionally search these paths for headers'),
        make_option('-L', '--lib-path',
            metavar='LIB_PATH',
            dest='lib_paths',
            action='append',
            help='additionally search these paths for libraries'),
        make_option('--build',
            dest='build_model',
            help='specify the build model'),
        make_option('--host',
            dest='host_model',
            help='specify the host model'),
        make_option('--target',
            dest='target_model',
            help='specify the target model'),
        make_option('--run',
            dest='run_model',
            help='specify the run model'),
        make_option('--buildcc',
            metavar='CC',
            help='specify the build c compiler'),
        make_option('--hostcc',
            metavar='CC',
            help='specify the host c compiler'),
        make_option('--targetcc',
            metavar='CC',
            help='specify the target c compiler'),
        make_option('--buildcxx',
            metavar='CXX',
            help='specify the build c++ compiler'),
        make_option('--hostcxx',
            metavar='CXX',
            help='specify the host c++ compiler'),
        make_option('--targetcxx',
            metavar='CXX',
            help='specify the target c++ compiler'),
        make_option('--boot',
            dest='bootfile',
            help='add a config bootfile for additional config modification'),
        make_option('--src-dir',
            metavar='PATH',
            default=os.environ.get("SRC_DIR", os.curdir),
            help='specify the source directory'),
        make_option('--lparchive',
            metavar='PATH',
            default=os.environ.get("FLX_LPARCHIVE", os.curdir),
            help='specify the location of the interscript files'),
        make_option('--phase',
            dest='phases',
            action='append',
            default=[],
            help='specify which phases to configure'),
    ])

    options, args = parser.parse_args()

    if not options.verbose:
        options.quiet = 2
    else:
        options.quiet = 0

    if options.build_model:
        print "Specified build model", options.build_model
        options.build_model = check_model(options.build_model)
    else:
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
                options.build_model = archmap[os.name]
            except KeyError:
                print "uname -s and Python returns unknown OS type, assuming 'detect'"
                options.build_model = "detect"
        else:
            output = output[0].strip().lower()
            options.build_model = archmap[output]

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
                options.build_model = archmap[os.name]
            except KeyError:
                print "uname -s and Python returns unknown OS type, assuming 'detect'"
                options.build_model = "detect"
        else:
            output = output[0].strip().lower()
            options.build_model = archmap[output]

    print "Build platform: " + options.build_model

    if options.host_model:
        print "Specified host model", options.host_model
        options.host_model = check_model(options.host_model)

    if options.target_model:
        print "Specified target model", options.target_model
        options.target_model = check_model(options.target_model)

    if options.run_model:
        print "Specified run model", options.run_model
        options.run_model = check_model(options.run_model)

    for phase in options.phases:
        if phase not in ALL_PHASES:
            print "UNKNOWN PHASE", phase,"not in", ALL_PHASES
            sys.exit(1)

    if not options.phases:
        options.phases = ALL_PHASES

    if options.bootfile:
        try:
            execfile(options.bootfile)
            print "Loaded", options.bootfile
        except:
            print "Cannot execute specified bootstrap file: ", options.bootfile
            sys.exit(1)

    return options, args

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

def load_compiler(options, compiler, cc, input_cc, phase, model, suffix):
    CC = compiler(verbose=options.verbose, quiet=options.quiet)

    CC.set_options(
        COM=cc,
        include_paths=options.include_paths,
        lib_paths=options.lib_paths,
        use=phase,
        model=model,
        build=options.build_model,
    )

    if input_cc and (
            input_cc.options.COM == CC.options.COM and
            input_cc.options.model == CC.options.model and
            input_cc.options.build == CC.options.build):
        print 'using the', input_cc.options.use, 'CC options for the', phase, 'CC'
        CC.load_options(os.path.join('config', input_cc.options.use + suffix))
        CC.options.use = phase
    else:
        CC.check_options()
        CC.report_config()
    CC.save_options(os.path.join('config', phase + suffix))

    return CC



def load_compilers(options, phase, model, cc, cxx,
        input_cc=None,
        input_cxx=None):
    print
    print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    print "Checking", phase, "MODEL", model
    print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    print

    if model in ["win32","win64"]:
        CC = fbuild.flxbuild.msvcc_class.msvcc
        CXX = fbuild.flxbuild.msvcxx_class.msvcxx
    else:
        CC = fbuild.flxbuild.gcc_class.gcc
        CXX = fbuild.flxbuild.gxx_class.gxx

    CC = load_compiler(options, CC, cc, input_cc, phase, model, '_cc.py')
    CXX = load_compiler(options, CXX, cxx, input_cxx, phase, model, '_cxx.py')

    return CC, CXX

# -----------------------------------------------------------------------------

def configure_build(options):
    BUILD_CC, BUILD_CXX = load_compilers(options,
        phase='build',
        model=options.build_model,
        cc=options.buildcc,
        cxx=options.buildcxx,
    )

    BUILD_CC.check_options()
    BUILD_CC.report_config()
    BUILD_CC.save_options("config/build_cc.py")
    options.build_model = BUILD_CC.options.model

    BUILD_CXX.check_options()
    BUILD_CXX.report_config()
    BUILD_CXX.save_options("config/build_cxx.py")
    options.build_model = BUILD_CXX.options.model

    cpkgs =  glob.glob("cpkgs"+os.sep+"build"+os.sep+"*.py")
    for cpkgf in cpkgs:
        cpkg = os.path.splitext(os.path.basename(cpkgf))[0]
        print "build CONFIGURING", cpkg
        __import__('cpkgs.build.' + cpkg)

    return BUILD_CC, BUILD_CXX


def configure_host(options, BUILD_CC, BUILD_CXX):
    #
    # Now create the host model: the compiler has to run
    # on the build machine, but can cross compile for
    # the host (if so, we can build but not test Felix)
    #
    # Cross compilation of the host tools may prevent any
    # testing of the tools
    #

    if not options.host_model:
        options.host_model = options.build_model
        print "Defaulting host model to build model:", options.host_model

    HOST_CC, HOST_CXX = load_compilers(options,
        phase='host',
        model=options.host_model,
        cc=options.hostcc,
        cxx=options.hostcxx,
        input_cc=BUILD_CC,
        input_cxx=BUILD_CXX,
    )

    HOST_OCAML = fbuild.flxbuild.ocaml_class.ocaml(verbose=options.verbose, quiet=options.quiet)

    camllinkopts = ""
    if HOST_CXX.options.CYGWIN:
        camllinkopts = '-ccopt "-Wl,--stack -Wl,10485760" '

    HOST_OCAML.autodetect(camllinkopts)
    HOST_OCAML.report_config()
    HOST_OCAML.save_options("config/ocaml_config.py")

    options.host_model = HOST_CXX.options.model

    cpkgs =  glob.glob("cpkgs"+os.sep+"host"+os.sep+"*.py")
    for cpkgf in cpkgs:
        cpkg = os.path.splitext(os.path.basename(cpkgf))[0]
        print "host CONFIGURING", cpkg
        __import__('cpkgs.host.' + cpkg)

    return HOST_CC, HOST_CXX, HOST_OCAML


def configure_target(options, HOST_CC, HOST_CXX):
    #
    # Now create the target model: the compiler has to run
    # on the build machine, but can cross compile for
    # the target
    #
    # cross compilation of C++ generated by Felix allows us to
    # check the generated code compiles, but not that it runs
    # [but the output is largely portable so we can still try]
    #

    if not options.target_model:
        options.target_model = options.host_model
        print "Defaulting target model to host model:", options.target_model

    TARGET_CC, TARGET_CXX = load_compilers(options,
        phase='target',
        model=options.target_model,
        cc=options.targetcc,
        cxx=options.targetcxx,
        input_cc=HOST_CC,
        input_cxx=HOST_CXX,
    )

    options.target_model = TARGET_CXX.options.model

    return TARGET_CC, TARGET_CXX


def configure_run(options):
    if not options.run_model:
        options.run_model = options.target_model
        print "Defaulting run model to target model:", options.run_model

# -----------------------------------------------------------------------------

def write_config(options, CONFIG_TIME,
        BUILD_CC, BUILD_CXX,
        HOST_CC, HOST_CXX, HOST_OCAML,
        TARGET_CC, TARGET_CXX):
    try:
        print "Writing main config file"
        f = open("config"+os.sep+"__init__.py","w")
        pr(f,'import sys')
        pr(f,"if '' not in sys.path: sys.path = [''] + sys.path")
        pr(f,'#'+CONFIG_TIME)
        pr(f,"CONFIG_TIME = " + repr(CONFIG_TIME))
        pr(f,"flx_version = " + repr(version.flx_version))
        pr(f,"flx_version_major = " + repr(version.flx_version_major))
        pr(f,"godi_revision = " + repr(version.godi_revision))
        pr(f,"debian_revision = " + repr(version.debian_revision))
        if options.bootfile:
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
        pr(f,"SUPPORT_DYNAMIC_LOADING = " + repr(TARGET_CXX.options.SUPPORT_DYNAMIC_LOADING))
        pr(f,"SUPPORT_STATIC_LINKAGE = 1")
        if TARGET_CXX.options.SUPPORT_DYNAMIC_LOADING:
            pr(f,"DEFAULT_LINK_MODEL = 'dynamic'")
        else:
            pr(f,"DEFAULT_LINK_MODEL = 'static'")
        pr(f,"build_model = " + repr(options.build_model))
        pr(f,"host_model = " + repr(options.host_model))
        pr(f,"target_model = " + repr(options.target_model))
        pr(f,"run_model = " + repr(options.run_model))

        # target model switches
        pr(f,"CYGWIN = " + repr(TARGET_CXX.options.CYGWIN))
        pr(f,"MACOSX = " + repr(TARGET_CXX.options.MACOSX))
        pr(f,"WIN32 = " + repr(TARGET_CXX.options.WIN32))
        pr(f,"WIN64 = " + repr(TARGET_CXX.options.WIN64))
        pr(f,"POSIX = " + repr(TARGET_CXX.options.POSIX))
        pr(f,"SOLARIS = " + repr(TARGET_CXX.options.SOLARIS))
        pr(f,"BSD = " + repr(TARGET_CXX.options.BSD))
        pr(f,"LINUX = " + repr(TARGET_CXX.options.LINUX))
        pr(f,"PREFIX = " + repr(options.prefix))
        pr(f,"src_dir = " + repr(options.src_dir))
        pr(f,"FLX_LPARCHIVE = " + repr(options.lparchive))
        pr(f,"FLX_RTL_DIR = " + repr(os.path.join('lib', 'rtl')))
        pr(f,"FLX_HOST_CONFIG_DIR = " + repr(os.path.join('config', 'host')))
        pr(f,"FLX_TARGET_CONFIG_DIR = " + repr(os.path.join('config', 'target')))
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

        if options.target_model in ["win32","win64"]:
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

        pr(f,"HAVE_GNU = " + repr(HAVE_GNU))
        pr(f,"FLXCC_CPP='cpp '")
        pr(f,"HAVE_MSVC = " + repr(HAVE_MSVC))
        pr(f,"DIFF = " + repr(DIFF))

        ISCR = sys.executable + ' ' + \
            os.path.join(options.src_dir, 'interscript', 'bin', 'iscr.py') + \
            ' --cache-prefix=lpsrc-cache'
        pr(f,"ISCR = " + repr(ISCR))

        # ---------------------------------------------------

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
        pr(f,"ARCH = " + repr(ARCH))

        try:
            if options.build_model == 'osx':
                output = xqt('uname -p')
            else:
                output = xqt('uname -o')
        except ExecutionError:
            OS = 'unknown'
        else:
            OS = output[0].strip().lower()

        pr(f,"OS = " + repr(OS))

        pr(f,"")
        pr(f,"# HACK to get all the target variables into global namespace")
        f.close()
        if options.bootfile:
            print "Copying bootfile :", options.bootfile
            shutil.copy(options.bootfile, os.path.join('config', 'config_bootstrap.py'))
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

# -----------------------------------------------------------------------------

def main():
    time_stamp_format = "%Y/%m/%d %H:%M:%S UTC"
    config_time = time.gmtime(time.time())
    CONFIG_TIME = time.strftime(time_stamp_format, config_time)

    options, args = load_options()

    if args:
        print 'Unknown configure args:', args
        return 1

    if not os.path.exists('config'):
        os.path.mkdir('config')

    BUILD_CC, BUILD_CXX = configure_build(options)
    HOST_CC, HOST_CXX, HOST_OCAML = configure_host(options, BUILD_CC, BUILD_CXX)
    TARGET_CC, TARGET_CXX = configure_target(options, HOST_CC, HOST_CXX)
    configure_run(options)

    write_config(options, CONFIG_TIME,
        BUILD_CC, BUILD_CXX,
        HOST_CC, HOST_CXX, HOST_OCAML,
        TARGET_CC, TARGET_CXX,
    )

    return 0

# -----------------------------------------------------------------------------

if __name__ == '__main__':
    sys.exit(main())

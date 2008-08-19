import os
import config

# make sure we have the runtime library in our search path
if config.CYGWIN or config.WIN32:
  pass
elif config.MACOSX:
  os.environ['DYLD_LIBRARY_PATH'] = \
      os.environ.get('DYLD_LIBRARY_PATH', '') + os.pathsep + \
      config.FLX_RTL_DIR
else:
  os.environ['LD_LIBRARY_PATH'] = \
      os.environ.get('LD_LIBRARY_PATH', '') + os.pathsep + \
      config.FLX_RTL_DIR

# -----------------------------------------------------------------------------

from optparse import make_option

options = [
  make_option('-O', '--optimise',
    action='store_true',
    default=True,
    help='generate optimised code'),
  make_option('--no-optimise', dest='optimise',
    action='store_false',
    help='turn off optimised code generation'),
  make_option('-g', '--debug',
    action='store_true',
    default=True,
    help='generate debugging information'),
  make_option('--no-debug', dest='debug',
    action='store_false',
    help='turn off debugging information generation'),
  make_option('-D', dest='macros', metavar='MACRO',
    action='append',
    default=[],
    help='define a macro'),
]

fbuild_preprocesses = [
  'fbuild.mkplugins.manifest.manifest',
]

ocaml_processes = ['fbuild.processes.ocaml.copy_mli2ml']

if config.HOST_OCAML.options.HAVE_OCAMLBUILD:
  ocaml_processes += ['fbuild.processes.ocamlbuild.build']
else:
  ocaml_processes += [
    'fbuild.processes.ocaml.build_grammar',
    'fbuild.processes.dypgen.build_grammar',
    'fbuild.processes.ocaml.build_modules',
    'fbuild.processes.ocaml.build_libs',
    'fbuild.processes.ocaml.build_exes',
  ]

fbuild_processes = {
  'build': [
    'fbuild.mkplugins.extract_iscr.extract_iscr',
  ],
  'host': ocaml_processes + [
    'fbuild.mkplugins.build_host_tools.build_host_tools',
    'fbuild.mkplugins.extract_grammar.extract_grammar',
  ],
  'target': [
    'fbuild.mkplugins.build_target_rtl_dynamic.build_target_rtl_dynamic',
    'fbuild.mkplugins.build_target_rtl_static.build_target_rtl_static',
    'fbuild.mkplugins.build_felix_dynamic_drivers.build_felix_dynamic_drivers',
    'fbuild.mkplugins.build_felix_static_drivers.build_felix_static_drivers',
    'fbuild.mkplugins.build_target_cpp_tools.build_target_cpp_tools',
    'fbuild.mkplugins.build_target_felix_rtl.build_target_felix_rtl',
    'fbuild.mkplugins.build_target_felix_tools.build_target_felix_tools',
  ],
  'run': [
    'fbuild.mkplugins.run_failure_tests.run_failure_tests',
    'fbuild.mkplugins.run_unit_tests.run_unit_tests',
    'fbuild.mkplugins.run_static_unit_tests.run_static_unit_tests',
    'fbuild.mkplugins.run_dynamic_unit_tests.run_dynamic_unit_tests',
    'fbuild.mkplugins.run_completion_tests.run_completion_tests',
    'fbuild.mkplugins.run_known_failed_tests.run_known_failed_tests',
  ],
  'doc': [
    'fbuild.mkplugins.mkdoc.mkdoc',
    'fbuild.mkplugins.impldoc.impldoc',
    'fbuild.mkplugins.rtldoc.rtldoc',
    'fbuild.mkplugins.gramdoc.gramdoc',
    'fbuild.mkplugins.typeclassdoc.typeclassdoc',
  ],
  'demo': [
    'fbuild.mkplugins.run_demos.run_demos',
  ],
  'speed': [
    'fbuild.mkplugins.speed_tests.speed_tests',
  ],
  'clean': [
    'fbuild.mkplugins.clean.clean',
  ],
  'clean_run': [
    'fbuild.mkplugins.clean_run.clean_run',
  ],
}

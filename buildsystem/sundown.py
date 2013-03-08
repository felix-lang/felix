import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path ('src/sundown')
    buildsystem.copy_hpps_to_rtl(phase.ctx,
        path / 'flx_sundown.hpp',
    )

    dst = 'lib/rtl/flx_sundown'
    suffix = '.so'
    srcs = [
      'src/sundown/autolink.cpp',
      'src/sundown/buffer.cpp',
      'src/sundown/markdown.cpp',
      'src/sundown/stack.cpp',
      'src/sundown/html.cpp',
      'src/sundown/houdini_href_e.cpp',
      'src/sundown/houdini_html_e.cpp',
      'src/sundown/flx_sundown.cpp']
    includes = [path,phase.ctx.buildroot / 'config/target']
    macros = ['BUILD_SUNDOWN']

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[]),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            libs=[]))

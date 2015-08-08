
import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path(phase.ctx.buildroot/'share'/'src/re2/re2')

    buildsystem.copy_to(phase.ctx, phase.ctx.buildroot / "share/lib/rtl/re2", [
        path / 're2/re2.h',
        path / 're2/set.h',
        path / 're2/stringpiece.h',
        path / 're2/variadic_function.h',
        ]
     )

    dst = 'host/lib/rtl/flx_re2'
    srcs = [
        path / 're2/bitstate.cc',
        path / 're2/compile.cc',
        path / 're2/dfa.cc',
        path / 're2/filtered_re2.cc',
        path / 're2/mimics_pcre.cc',
        path / 're2/nfa.cc',
        path / 're2/onepass.cc',
        path / 're2/parse.cc',
        path / 're2/perl_groups.cc',
        path / 're2/prefilter.cc',
        path / 're2/prefilter_tree.cc',
        path / 're2/prog.cc',
        path / 're2/re2.cc',
        path / 're2/regexp.cc',
        path / 're2/set.cc',
        path / 're2/simplify.cc',
        path / 're2/tostring.cc',
        path / 're2/unicode_casefold.cc',
        path / 're2/unicode_groups.cc',
        path / 'util/arena.cc',
        #path / 'util/benchmark.cc',
        path / 'util/hash.cc',
        #path / 'util/pcre.cc',
        #path / 'util/random.cc',
        path / 'util/rune.cc',
        path / 'util/stringpiece.cc',
        path / 'util/stringprintf.cc',
        path / 'util/strutil.cc',
        #path / 'util/thread.cc',
        path / 'util/valgrind.cc',
     ]
    includes = [
      phase.ctx.buildroot / 'host/lib/rtl',
      path ]
    macros = ['BUILD_RE2'] + (['WIN32', 'NOMINMAX'],[])[not 'win32' in phase.platform]
    cflags = ([], ['-Wno-sign-compare'])[not 'win32' in phase.platform]
    lflags = []
    libs = []
    external_libs = []

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=cflags,
            libs=libs,
            external_libs=external_libs,
            lflags=lflags),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=cflags,
            libs=libs,
            external_libs=external_libs,
            lflags=lflags))

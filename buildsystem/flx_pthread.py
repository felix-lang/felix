import fbuild
from fbuild.path import Path
from fbuild.record import Record
from fbuild.builders.file import copy

import buildsystem
from buildsystem.config import config_call

# ------------------------------------------------------------------------------

def build_runtime(phase):
    print('[fbuild] [rtl] build pthread')
    path = Path(phase.ctx.buildroot/'share'/'src/pthread')

 
    srcs = [
        path / 'pthread_win_posix_condv_emul.cpp', # portability hackery
        path / 'pthread_mutex.cpp',
        path / 'pthread_condv.cpp',
        path / 'pthread_counter.cpp',
        path / 'pthread_waitable_bool.cpp',
        path / 'pthread_semaphore.cpp',
        path / 'pthread_monitor.cpp',
        path / 'pthread_thread_control.cpp',
        path / 'pthread_win_thread.cpp',
        path / 'pthread_posix_thread.cpp',
    ]
    includes = [
      phase.ctx.buildroot / 'host/lib/rtl', 
      phase.ctx.buildroot / 'share/lib/rtl']
    macros = ['BUILD_PTHREAD']
    flags = []
    libs = []
    external_libs = []

    pthread_h = config_call('fbuild.config.c.posix.pthread_h',
        phase.platform,
        phase.cxx.shared)

    dst = 'host/lib/rtl/flx_pthread'
    if pthread_h.pthread_create:
        flags.extend(pthread_h.flags)
        libs.extend(pthread_h.libs)
        external_libs.extend(pthread_h.external_libs)

    return Record(
        static=buildsystem.build_cxx_static_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=flags,
            libs=libs,
            external_libs=external_libs,
            lflags=flags),
        shared=buildsystem.build_cxx_shared_lib(phase, dst, srcs,
            includes=includes,
            macros=macros,
            cflags=flags,
            libs=libs,
            external_libs=external_libs,
            lflags=flags))


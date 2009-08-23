import fbuild
from fbuild.functools import call
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/pthread')

    buildsystem.copy_hpps_to_rtl(phase.ctx,
        phase.ctx.buildroot / 'config/target/flx_pthread_config.hpp',

        # portable
        path / 'pthread_thread.hpp',
        path / 'pthread_mutex.hpp',
        path / 'pthread_counter.hpp',
        path / 'pthread_waitable_bool.hpp',
        path / 'pthread_condv.hpp',
        path / 'pthread_semaphore.hpp',
        path / 'pthread_monitor.hpp',
        path / 'pthread_sleep_queue.hpp',
        path / 'pthread_work_fifo.hpp',

        # win32 and posix
        path / 'pthread_win_posix_condv_emul.hpp',
    )

    dst = 'lib/rtl/flx_pthread'
    srcs = [
        path / 'pthread_win_posix_condv_emul.cpp', # portability hackery
        path / 'pthread_mutex.cpp',
        path / 'pthread_condv.cpp',
        path / 'pthread_counter.cpp',
        path / 'pthread_waitable_bool.cpp',
        path / 'pthread_semaphore.cpp',
        path / 'pthread_monitor.cpp',
        path / 'pthread_sleep_queue.cpp',
        path / 'pthread_work_fifo.cpp',
        path / 'pthread_thread_control.cpp',
    ]
    includes = [phase.ctx.buildroot / 'config/target', 'src/rtl']
    macros = ['BUILD_PTHREAD']
    flags = []
    libs = []
    external_libs = []

    if 'win32' in phase.platform:
        srcs.append(path / 'pthread_win_thread.cpp')

    if 'posix' in phase.platform:
        srcs.append(path / 'pthread_posix_thread.cpp')

    pthread_h = call('fbuild.config.c.posix.pthread_h', phase.cxx.shared)
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

def build_flx(phase):
    buildsystem.copy_flxs_to_lib(phase.ctx, Path('src/pthread/*.flx').glob())

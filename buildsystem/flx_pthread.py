import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src/pthread')
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
    libs = []
    flags = []

    if 'win32' in phase.platform:
        srcs.append(path / 'pthread_win_thread.cpp')

    if 'posix' in phase.platform:
        srcs.append(path / 'pthread_posix_thread.cpp')

    if 'linux' in phase.platform:
        pass

    if 'bsd' in phase.platform:
        pass

    if 'solaris' in phase.platform:
        libs.append('rt')

    try:
        pthread_h = env.config('fbuild.builders.c.posix.config_pthread_h',
            phase.cxx.shared)
    except fbuild.builders.ConfigFailed:
        pass
    else:
        flags.extend(pthread_h.flags)

    return cxx.SharedLibrary(path / 'flx_pthread', srcs,
        includes=[
            fbuild.Path('src', 'rtl'),
            fbuild.buildroot / 'config/target',
        ],
        libs=libs,
        cflags={'macros': ['BUILD_PTHREAD']},
        lflags={'flags': flags},
        builder=phase.cxx)

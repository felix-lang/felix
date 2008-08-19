import fbuild
import fbuild.packages.cxx as cxx

def build(env, phase):
    path = fbuild.Path('src', 'demux')
    srcs = [path / '*.cpp']
    includes = [
        fbuild.buildroot / 'config/target',
        fbuild.Path('src', 'pthread'),
        path,
    ]
    libs = [env.config('buildsystem.flx_pthread.build', phase)]

    if 'win32' in phase.platform:
        srcs.extend((
            path / 'win/demux_iocp_demuxer.cpp',       # windows
            path / 'win/demux_overlapped.cpp',         # windows
            path / 'win/demux_wself_piper.cpp',        # windows
            path / 'win/demux_win_timer_queue.cpp',    # windows
        ))
        includes.append(path / 'win')
        libs.extend(('ws2_32', 'mswsock'))

    if 'posix' in phase.platform:
        srcs.extend((
            path / 'posix/demux_posix_demuxer.cpp',      # posix
            path / 'posix/demux_select_demuxer.cpp',     # posix
            path / 'posix/demux_posix_timer_queue.cpp',  # posix
            path / 'posix/demux_sockety.cpp',            # posix
            path / 'posix/demux_self_piper.cpp',         # posix
            path / 'posix/demux_pfileio.cpp',            # posix
            path / 'posix/demux_ts_select_demuxer.cpp',  # posix
        ))
        includes.append(path / 'posix')

    try:
        env.config('fbuild.builders.c.posix.config_poll_h', phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        srcs.extend((
            # I've seen poll on linux and osx10.4 systems.
            # conditionally compiled and used.
            path / 'poll/demux_poll_demuxer.cpp',       # I've seen this on linux and osx10.4
            path / 'poll/demux_ts_poll_demuxer.cpp',    # ditto
        ))
        includes.append(path / 'poll')

    try:
        epoll = env.config('fbuild.builders.c.linux.config_sys_epoll_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if epoll:
            srcs.append(path / 'epoll/demux_epoll_demuxer.cpp')
            includes.append(path / 'epoll')

    try:
        kqueue = env.config('fbuild.builders.c.bsd.config_sys_event_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if kqueue:
            srcs.append(path / 'kqueue/demux_kqueue_demuxer.cpp')
            includes.append(path / 'kqueue')

    try:
        evtports = env.config('fbuild.builders.c.solaris.config_port_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if evtports:
            srcs.append(path / 'evtport/demux_evtport_demuxer.cpp')
            includes.append(path / 'evtport')

    return cxx.SharedLibrary(path / 'demux', srcs,
        includes=includes,
        libs=libs,
        cflags={'macros': ['BUILD_DEMUX']},
        builder=phase.cxx,
    )

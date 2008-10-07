import fbuild
from fbuild.path import Path
from fbuild.record import Record

import buildsystem

# ------------------------------------------------------------------------------

def build_runtime(phase):
    path = Path('src/demux')

    buildsystem.copy_hpps_to_rtl(
        fbuild.buildroot / 'config/target/flx_demux_config.hpp', # portable

        # portable
        path / 'flx_demux.hpp',
        path / 'demux_demuxer.hpp',
        path / 'demux_timer_queue.hpp',
        path / 'demux_quitter.hpp',

        # windows (monolithic)
        path / 'win/demux_iocp_demuxer.hpp',
        path / 'win/demux_overlapped.hpp',
        path / 'win/demux_win_timer_queue.hpp',
        path / 'win/demux_wself_piper.hpp',

        # posix
        path / 'posix/demux_posix_demuxer.hpp',
        path / 'posix/demux_posix_timer_queue.hpp',
        path / 'posix/demux_pfileio.hpp',
        path / 'posix/demux_select_demuxer.hpp',
        path / 'posix/demux_sockety.hpp',
        path / 'posix/demux_self_piper.hpp',
        path / 'posix/demux_ts_select_demuxer.hpp',

        # linux, osx 10.3 (select impl), 10.4 real.
        path / 'poll/demux_poll_demuxer.hpp',
        path / 'poll/demux_ts_poll_demuxer.hpp',

        # linux (>= 2.6)
        path / 'epoll/demux_epoll_demuxer.hpp',

        # osx (10.3 onwards)/bsd
        path / 'kqueue/demux_kqueue_demuxer.hpp',

        # solaris (9 onwards?)
        path / 'evtport/demux_evtport_demuxer.hpp',
    )

    dst = fbuild.buildroot / 'lib/rtl/demux'
    srcs = [path / '*.cpp']
    includes = [
        fbuild.buildroot / 'config/target',
        Path('src', 'pthread'),
        path,
    ]
    macros = ['BUILD_DEMUX']
    libs = [fbuild.env.run('buildsystem.flx_pthread.build_runtime', phase)]
    extra_libs = []

    if 'win32' in phase.platform:
        srcs.extend((
            path / 'win/demux_iocp_demuxer.cpp',       # windows
            path / 'win/demux_overlapped.cpp',         # windows
            path / 'win/demux_wself_piper.cpp',        # windows
            path / 'win/demux_win_timer_queue.cpp',    # windows
        ))
        includes.append(path / 'win')
        extra_libs.extend(('ws2_32', 'mswsock'))

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
        fbuild.env.cache('fbuild.builders.c.posix.config_poll_h', phase.cxx.shared)
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
        epoll = fbuild.env.cache('fbuild.builders.c.linux.config_sys_epoll_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if epoll:
            srcs.append(path / 'epoll/demux_epoll_demuxer.cpp')
            includes.append(path / 'epoll')

    try:
        kqueue = fbuild.env.cache('fbuild.builders.c.bsd.config_sys_event_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if kqueue:
            srcs.append(path / 'kqueue/demux_kqueue_demuxer.cpp')
            includes.append(path / 'kqueue')

    try:
        evtports = fbuild.env.cache('fbuild.builders.c.solaris.config_port_h',
            phase.cxx.shared)
    except fbuild.ConfigFailed:
        pass
    else:
        if evtports:
            srcs.append(path / 'evtport/demux_evtport_demuxer.cpp')
            includes.append(path / 'evtport')

    return Record(
        static=phase.cxx.static.build_lib(dst + '_static', srcs,
            includes=includes,
            macros=macros + ['FLX_STATIC_LINK'],
            libs=[lib.static for lib in libs] + extra_libs),
        shared=phase.cxx.shared.build_lib(dst + '_dynamic', srcs,
            includes=includes,
            macros=macros,
            libs=[lib.shared for lib in libs] + extra_libs))

def build_flx(builder):
    return buildsystem.copy_flxs_to_lib(Path('src/demux/*.flx').glob())

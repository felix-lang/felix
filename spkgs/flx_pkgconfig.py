import os

#
# we can't use flx_pkgconfig to supply platform
# libraries .. because this IS flx_pkgconfig --
# so it isn't built yet!!
#
# driver apps (like pkg_config) only use async if they have to, so I think
# all this socket bootstrapping stuff is now unnecessary. Let's see!
FLXFLAGS = ""
#if WIN32:
#  if HAVE_MSVC:
#    FLXFLAGS = "/DEFAULTLIB:ws2_32 /DEFAULTLIB:mswsock "
#  else:
#    FLXFLAGS = "-lws2_32 -lmswsock "
#elif SOLARIS:
#  FLXFLAGS = "-lsocket -lnsl " # uses async hooker->faio->demux->sockets

# DRLIBS = ['libflx_async','libfaio','libdemux','libflx_pthread','libflx', 'libflx_exceptions']
DRLIBS = ['libflx_pthread','libflx','libflx_gc','libflx_judy', 'libflx_exceptions']

pkg_requires = ['flx_compiler','flx_drivers','flx_stdlib']
felix_tools = [('src/flx_pkgconfig/flx_pkgconfig','bin/flx_pkgconfig')]
felix_requires_linkflags = FLXFLAGS
exes_require_libs = DRLIBS

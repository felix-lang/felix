

================
Platform configs
================


OSX
===


.. code-block:: cpp

   #ifndef __DEMUX_SOCKETY_CONFIG_H__
   #define __DEMUX_SOCKETY_CONFIG_H__
   #include <sys/socket.h>
   typedef socklen_t FLX_SOCKLEN_T;
   #endif


.. code-block:: cpp

   #ifndef __FLX_RTL_CONFIG_PARAMS_H__
   #define __FLX_RTL_CONFIG_PARAMS_H__
   
   #define FLX_HAVE_VSNPRINTF 1
   #define FLX_HAVE_GNU 1
   #define FLX_HAVE_GNU_BUILTIN_EXPECT 1
   #define FLX_HAVE_CGOTO 1
   #define FLX_HAVE_ASM_LABELS 0
   #define FLX_HAVE_DLOPEN 1
   #define FLX_CYGWIN 0
   #define FLX_MACOSX 1
   #define FLX_LINUX 0
   #define FLX_WIN32 0
   #define FLX_WIN64 0
   #define FLX_POSIX 1
   #define FLX_SOLARIS 0
   #define FLX_HAVE_MSVC 0
   #define FLX_HAVE_KQUEUE_DEMUXER 1
   #define FLX_HAVE_POLL 1
   #define FLX_HAVE_EPOLL 0
   #define FLX_HAVE_EVTPORTS 0
   #define FLX_HAVE_OPENMP 0
   #define FLX_MAX_ALIGN 16
   #endif

Linux
=====


.. code-block:: cpp

   #ifndef __DEMUX_SOCKETY_CONFIG_H__
   #define __DEMUX_SOCKETY_CONFIG_H__
   #include <sys/socket.h>
   typedef socklen_t FLX_SOCKLEN_T;
   #endif


.. code-block:: cpp

   #ifndef __FLX_RTL_CONFIG_PARAMS_H__
   #define __FLX_RTL_CONFIG_PARAMS_H__
   
   #define FLX_HAVE_VSNPRINTF 1
   #define FLX_HAVE_GNU 1
   #define FLX_HAVE_GNU_BUILTIN_EXPECT 1
   #define FLX_HAVE_CGOTO 1
   #define FLX_HAVE_ASM_LABELS 1
   #define FLX_HAVE_DLOPEN 1
   #define FLX_CYGWIN 0
   #define FLX_MACOSX 0
   #define FLX_LINUX 1
   #define FLX_WIN32 0
   #define FLX_WIN64 0
   #define FLX_POSIX 1
   #define FLX_SOLARIS 0
   #define FLX_HAVE_MSVC 0
   #define FLX_HAVE_KQUEUE_DEMUXER 0
   #define FLX_HAVE_POLL 1
   #define FLX_HAVE_EPOLL 1
   #define FLX_HAVE_EVTPORTS 0
   #define FLX_HAVE_OPENMP 1
   #define FLX_MAX_ALIGN 16
   #endif


Windows
=======




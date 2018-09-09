Package: src/packages/configs.fdoc

======================================== ============================================================
key                                      file                                                         
======================================== ============================================================
macosx64_demux_sockety_config.hpp        $PWD/src/config/macosx64/rtl/demux_sockety_config.hpp        
macosx64_clang_flx_rtl_config_params.hpp $PWD/src/config/macosx64/clang/rtl/flx_rtl_config_params.hpp 
linux64_demux_sockety_config.hpp         $PWD/src/config/linux64/rtl/demux_sockety_config.hpp         
linux64_gcc_flx_rtl_config_params.hpp    $PWD/src/config/linux64/gcc/rtl/flx_rtl_config_params.hpp    
======================================== ============================================================


================
Platform configs
================


OSX
===


.. code-block:: cpp

  //[macosx64_demux_sockety_config.hpp ]
  #ifndef __DEMUX_SOCKETY_CONFIG_H__
  #define __DEMUX_SOCKETY_CONFIG_H__
  #include <sys/socket.h>
  typedef socklen_t FLX_SOCKLEN_T;
  #endif


.. code-block:: cpp

  //[macosx64_clang_flx_rtl_config_params.hpp ]
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

  //[linux64_demux_sockety_config.hpp ]
  #ifndef __DEMUX_SOCKETY_CONFIG_H__
  #define __DEMUX_SOCKETY_CONFIG_H__
  #include <sys/socket.h>
  typedef socklen_t FLX_SOCKLEN_T;
  #endif


.. code-block:: cpp

  //[linux64_gcc_flx_rtl_config_params.hpp ]
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



.. code-block:: cpp

  //[linux64_gcc_flx_rtl_config_params.hpp ]

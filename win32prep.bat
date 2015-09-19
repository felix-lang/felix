	@echo off
	echo [1] Create directories
	rm -rf build/release/win32 tmp-dir trial-tmp
	md build\release\win32\bin
	md build\release\win32\config
	md build\release\win32\lib\rtl
	md build\release\win32\lib\plat
	echo [2] Copy compiler
	copy build\release\host\bin\flxg.exe build\release\win32\bin >nuk 2>&1
	echo [3] Copy resource database for Windows
	copy src\config\*.fpc build\release\win32\config >nul 2>&1
	copy src\config\win32\*.fpc build\release\win32\config >nul 2>&1
	echo [4] Set the toolchain
	echo "toolchain: toolchain_msvc_win32" > build/release/win32/config/toolchain.fpc
	echo [5] Set the C++ configuration parameters
	echo // From Makefile: win32-prep           > build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #ifndef __FLX_RTL_CONFIG_PARAMS_H__    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define __FLX_RTL_CONFIG_PARAMS_H__    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo //                                       >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_VSNPRINTF 1           >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_GNU 0                 >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_GNU_BUILTIN_EXPECT 0  >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_CGOTO 0               >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #efine FLX_HAVE_ASM_LABELS 0          >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_DLOPEN 0              >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_CYGWIN 0                   >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_MACOSX 0                   >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_LINUX 0                    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_WIN32 1                    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_WIN64 1                    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_POSIX 0                    >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_SOLARIS 0                  >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_MSVC 1                >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_KQUEUE_DEMUXER 0      >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_POLL 0                >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_EPOLL 0               >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_EVTPORTS 0            >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_HAVE_OPENMP 0              >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #define FLX_MAX_ALIGN 16               >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo //                                      >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo #endif                                 >> build\release\win32\lib\rtl\flx_rtl_config_params.hpp
	echo [6] Set the Felix platform config parameters
	echo // From Makefile: win32-prep           > build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_WIN32 = true;           >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_POSIX = false;          >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_LINUX = false;          >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_MACOSX = false;         >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_CYGWIN = false;         >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_SOLARIS = false;        >> build\release\win32\lib\plat\flx.flxh
	echo macro val PLAT_BSD = false;            >> build\release\win32\lib\plat\flx.flxh
	echo [7] Set the Felix floating point weirdnesses (nans and such like)
	copy build\release\host\lib\plat\float.flx build\release\win32\lib\plat\float.flx >nul 2>&1

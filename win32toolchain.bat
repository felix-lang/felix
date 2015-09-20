md win32flxdir
md mycache
build\release\host\bin\flx --felix=win32.fpc --force --force-compiler --target-dir=build/release/win32 --cache-dir=mycache -c --nolink --nocc --bundle-dir=win32flxdir  build/release/share/lib/plugins/toolchain_msvc_win32.flx
cl.exe win32flxdir\toolchain_msvc_win32.cpp /MDd /D_ITERATOR_DEBUG_LEVEL=2 /c /EHs /Iwin32flxdir /Ibuild\release\win32\lib\rtl /Ibuild\release\share\lib\rtl /Fowin32flxdir\toolchain_msvc_win32.obj
cl.exe /MDd win32flxdir\toolchain_msvc_win32.obj  /Fewin32flxdir\toolchain_msvc_win32.dll /link /DLL /DEBUG /LIBPATH:build\release\win32\lib\rtl /DEFAULTLIB:flx_gc_dynamic /DEFAULTLIB:flx_strutil_dynamic /DEFAULTLIB:flx_exceptions_dynamic /DEFAULTLIB:flx_dynamic /DEFAULTLIB:flx_pthread_dynamic /DEFAULTLIB:flx_dynlink_dynamic /DEFAULTLIB:flx_re2_dynamic /DEFAULTLIB:judy_dynamic
copy win32flxdir\toolchain_msvc_win32.dll build\release\win32\lib\rtl
copy win32flxdir\toolchain_msvc_win32.lib build\release\win32\lib\rtl

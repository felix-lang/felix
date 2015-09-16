build\release\host\bin\flx --felix=win32.fpc --static --cache-dir=mycache -c --nolink --bundle-dir=win32flxdir --force --force-compiler --nocc hello.flx
cl.exe win32flxdir\hello.cpp /MTd /DFLX_STATIC_LINK /D_ITERATOR_DEBUG_LEVEL=2 /c /EHs /Ibuild\release\win32\lib\rtl /Ibuild\release\share\lib\rtl /Fowin32flxdir\hello.obj
cl.exe win32flxdir\hello_static_link_thunk.cpp /MTd /D_ITERATOR_DEBUG_LEVEL=2 /DFLX_STATIC_LINK /c /EHs /Ibuild\release\win32\lib\rtl /Ibuild\release\share\lib\rtl /Fowin32flxdir\hello_static_link_thunk.obj
cl.exe /MTd win32flxdir\hello.obj win32flxdir\hello_static_link_thunk.obj build\release\win32\lib\rtl\flx_run_lib_static.obj build\release\win32\lib\rtl\flx_run_main_static.obj /Fewin32flxdir\hello.exe /link /LIBPATH:build\release\win32\lib\rtl /DEFAULTLIB:flx_gc_static /DEFAULTLIB:flx_strutil_static /DEFAULTLIB:flx_exceptions_static /DEFAULTLIB:flx_static /DEFAULTLIB:flx_pthread_static /DEFAULTLIB:flx_dynlink_static /DEFAULTLIB:flx_re2_static /DEFAULTLIB:judy_static
type "Running Hello"
win32flxdir\hello.exe

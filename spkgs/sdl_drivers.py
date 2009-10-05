import cpkgs.target.sdl as sdl

if sdl.HAVE_SDL:
  STATIC_DRIVERS = [
    ('src/flx_drivers/flx_arun', 'lib/rtl'),
  ]
  DYNAMIC_DRIVERS = [
    ('src/flx_drivers/flx_arun', 'bin'),
  ]
  DRLIBS = [
   'libflx_async',
   'libfaio',
   'libdemux',
   'libflx_pthread',
   'libflx',
   'libflx_gc',
   'libflx_judy',
   ]

  include_path = ['src/rtl']

  static_drivers = STATIC_DRIVERS
  dynamic_drivers = DYNAMIC_DRIVERS
  drivers_require_libs = DRLIBS
  pkg_requires = ['flx_rtl','flx_async','faio','demux','flx_pthread','sdl']
  cflags = sdl.SDL_CFLAGS
  dflags = sdl.SDL_LIBS
  sflags = sdl.SDL_STATIC_LIBS

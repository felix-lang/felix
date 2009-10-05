import os

import config
import cpkgs.target.sdl as sdl

if sdl.HAVE_SDL:
  # flags are compiler natives switches
  # not to be confused with platform independent lists
  # of libraries, include files, paths, etc
  cflags = sdl.SDL_CFLAGS
  dflags = sdl.SDL_LIBS
  sflags = sdl.SDL_STATIC_LIBS

  pkg_requires = ['faio']
  lib_requires = ['libfaio', 'libdemux','libflx_pthread','libflx']
  root = config.src_dir
  demos = [('demos', 'sdl', '*.flx')]

iscr_source = ['lpsrc/flx_sdl.pak']
build_macro = "SDL"
weaver_directory = "doc/sdl/"

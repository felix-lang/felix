try:
  from config.sdl_config import *
except ImportError:
  import os
  import config
  from fbuild.flxbuild.config_support import cwrite, pa
  from fbuild.flxbuild.flxutil import xqt, ExecutionError, file_exists

  try:
    SDL_VERSION = xqt("sdl-config --version")[0].strip()
  except ExecutionError:
    HAVE_SDL = 0
    SDL_VERSION = "None"
    SDL_CFLAGS = ""
    SDL_LIBS = ""
    SDL_STATIC_LIBS = ""
    print "SDL NOT SUPPORTED"
  else:
    print "SDL SUPPORTED, version ",SDL_VERSION
    HAVE_SDL = 1
    SDL_CFLAGS = xqt("sdl-config --cflags")[0].strip()
    SDL_LIBS = xqt("sdl-config --libs")[0].strip()
    SDL_STATIC_LIBS = xqt("sdl-config --static-libs")[0].strip()

    fname = "config" + os.sep + "sdl.fpc"
    if not file_exists(fname):
      print "Creating config/sdl.fpc"
      f = open (fname,"w")
      f.write("Name: SDL\n")
      f.write("Description: Simple Direct Media Layer\n")
      f.write("Version: "+SDL_VERSION+"\n")
      f.write("cflags: "+SDL_CFLAGS+"\n")
      f.write("requires_dlibs: '"+SDL_LIBS+"'\n")
      f.write("requires_slibs: '"+SDL_STATIC_LIBS+"'\n")
      f.write("flx_requires_driver: flx_arun\n")
      f.close()
    fname = "config" + os.sep + "gl.fpc"
    if not file_exists(fname):
      print "Creating config/gl.fpc"
      f = open (fname,"w")
      f.write("Name: OpenGL\n")
      f.write("Description: open GL graphics\n")
      if config.MACOSX:
        f.write("provides_dlib: -framework OpenGL\n")
        f.write("provides_slib: -framework OpenGL\n")
      else:
        f.write("provides_dlib: -lGL\n")
        f.write("provides_slib: -lGL\n")
      f.close()
    fname = "config" + os.sep + "glu.fpc"
    if not file_exists(fname):
      print "Creating config/glu.fpc"
      f = open (fname,"w")
      f.write("Name: GLU\n")
      f.write("Description: GLU graphics \n")
      if config.MACOSX:
        f.write("provides_dlib: -framework OpenGL\n")
        f.write("provides_slib: -framework OpenGL\n")
      else:
        f.write("provides_dlib: -lGLU\n")
        f.write("provides_slib: -lGLU\n")
      f.write("Requires: gl\n")
      f.close()
    fname = "config" + os.sep + "glut.fpc"
    if not file_exists(fname):
      print "Creating config/glut.fpc"
      f = open (fname,"w")
      f.write("Name: GLUT\n")
      f.write("Description: GLUT graphics\n")
      if config.MACOSX:
        f.write("provides_dlib: -framework GLUT\n")
        f.write("provides_slib: -framework GLUT\n")
      else:
        f.write("provides_dlib: -lglut\n")
        f.write("provides_slib: -lglut\n")
      f.write("Requires: gl glu\n")
      f.close()

  print "SDL cpkgs/target/sdl.py"
  f = cwrite("sdl")
  pa(f,locals(),"HAVE_SDL")
  pa(f,locals(),"SDL_VERSION")
  pa(f,locals(),"SDL_CFLAGS")
  pa(f,locals(),"SDL_LIBS")
  pa(f,locals(),"SDL_STATIC_LIBS")
  f.close()

import os
try:
  from config.target_mpi_config import *
except ImportError:
  from fbuild.flxbuild.config_support import cwrite, pa, locate_file

  #HAVE_MPI= config.TARGET_CXX.check_header_exists('mpi.h')
  # THIS ONLY WORKS ON UNIX
  print "Search for include/mpi.h"
  dir = locate_file("/include/mpi.h")
  print "Result of search",dir
  if dir:
    f = open("config"+os.sep+"mpi.fpc","w")
    if dir != "/usr":
      f.write("cflags: -I"+dir+"/include\n")
    f.write("provides_dlib: -lmpi\n")
    f.write("provides_slib: -lmpi\n")
    f.close()
    HAVE_MPI=1
  else:
    HAVE_MPI=0

  f = cwrite('target_mpi')
  pa(f, locals(), "HAVE_MPI")
  f.close()

from cpkgs.target.mpi import HAVE_MPI
if HAVE_MPI:
  iscr_source = ['lpsrc/mpi.pak']
  weaver_directory = 'doc/rtl/mpi/'

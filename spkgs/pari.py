from cpkgs.target.pari import HAVE_PARI

if HAVE_PARI:
  iscr_source = ['lpsrc/pari.pak']
  weaver_directory = 'doc/rtl/pari/'

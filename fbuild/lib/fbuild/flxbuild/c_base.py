import os

from fbuild.flxbuild.c_cxx_base import c_cxx_base

class c_base(c_cxx_base):
  def __init__(self, *args, **kwds):
    super(c_base, self).__init__(*args, **kwds)

    self.options.EXT_SRC_MAIN = ".c"
    self.options.EXT_SRC_LIB = ".c"

  def report_isnan(self):
    opt = self.options

    if opt.HAVE_C99_ISNAN_IN_MATH:
      print "C99 NaN Support in                      : <math.h>"
    elif opt.HAVE_BSD_ISNAN_IN_MATH:
      print "BSD NaN Support in                      : <math.h>"
    elif opt.HAVE_BSD_ISNAN_IN_IEEEFP:
      print "BSD NaN Support in                      : <ieeefp.h>"
    else:
      print "NaN                                     : EMULATED"

    if opt.HAVE_C99_ISINF_IN_MATH:
      print "C99 INF Support in                      : <math.h>"
    elif opt.HAVE_BSD_ISINF_IN_MATH:
      print "BSD INF Support in                      : <math.h>"
    elif opt.HAVE_BSD_ISINF_IN_IEEEFP:
      print "BSD INF Support in                      : <ieeefp.h>"
    else:
      print "INF                                     : EMULATED"

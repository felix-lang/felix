import os

from fbuild.flxbuild.c_cxx_base import c_cxx_base
from fbuild.flxbuild.flxutil import ExecutionError

class cxx_base(c_cxx_base):
  def __init__(self, *args, **kwds):
    super(cxx_base, self).__init__(*args, **kwds)

    self.options.EXT_SRC_MAIN = ".cxx"
    self.options.EXT_SRC_LIB = ".cpp"


  def detect_static_initialization(self):
    # find if we have static const init in class
    # [not so much an extension as a bug if we don't]
    try:
      self.build_string_program(r"""
struct X {
  static const int i = 1;
};

int main(int argc, char** argv) {
  return 0;
}
""", 'tmp' + os.sep + 'check_inclass')
      self.options.HAVE_INCLASS_MEMBER_INITIALIZATION = True
      print "Inclass member initialisation supported"
    except ExecutionError:
      self.options.HAVE_INCLASS_MEMBER_INITIALIZATION = False
      print "Inclass member initialisation NOT supported"


  def detect_cmath_isnan(self):
    # find if we have BSD isnan in <cmath> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <cmath>

int main(int argc, char** argv) {
  float f = 0.0;
  std::isnan(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_cmath')
      self.options.HAVE_CXX_ISNAN_IN_CMATH = True
      print "C++ isnan found in <cmath>"
    except ExecutionError:
      self.options.HAVE_CXX_ISNAN_IN_CMATH = False

    # find if we have BSD isinf in <cmath> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <cmath>

int main(int argc, char** argv) {
  float f = 0.0;
  std::isinf(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_cmath')
      self.options.HAVE_CXX_ISINF_IN_CMATH = True
      print "C++ isinf found in <cmath>"
    except ExecutionError:
      self.options.HAVE_CXX_ISINF_IN_CMATH = False

    # find if we have BSD isinf in <cmath> (NAUGHTY!)
    try:
      self.build_string_program(r"""
#include <cmath>

int main(int argc, char** argv) {
  float f = 0.0;
  std::isfinite(f);
  return 0;
}
""", 'tmp' + os.sep + 'nan_cmath')
      self.options.HAVE_CXX_ISFINITE_IN_CMATH = True
      print "C++ isfinite found in <cmath>"
    except ExecutionError:
      self.options.HAVE_CXX_ISFINITE_IN_CMATH = False



  def check_options(self):
    super(cxx_base, self).check_options()

    self.detect_static_initialization()
    self.detect_cmath_isnan()

  def report_isnan(self):
    opt = self.options

    if opt.HAVE_CXX_ISNAN_IN_CMATH:
      print "C++ isnan Support in                        : <cmath>"
    else:
      print "C++ isnan                                   : EMULATED"

    if opt.HAVE_CXX_ISINF_IN_CMATH:
      print "C++ isinf Support in                        : <cmath>"
    elif opt.HAVE_CXX_ISFINITE_IN_CMATH:
      print "C++ isfinite Support in                     : <cmath>"
    else:
      print "C++ isinf                                   : EMULATED"

import os
import sys

import fbuild.flxbuild
from fbuild.flxbuild.cxx_base import cxx_base
from fbuild.flxbuild.gnu_mixin import gnu_mixin

class gxx(gnu_mixin, cxx_base):
  DEFAULT_COM = 'g++'

  def check_options(self):
    cxx_base.check_options(self)
    gnu_mixin.check_options(self)

    self.detect_gxx_template_extensions()


  def detect_gxx_template_extensions(self):
    # find if we have g++ supported ext/ with STL extensions
    try:
      self.build_string_program(r"""
#include <iostream>

// we only bother to check the include file exists
#include <ext/hash_map>
using namespace __gnu_cxx;

int main(int argc,char** argv) {
   return 0;
}
""", 'tmp' + os.sep + 'gnu_hash')
      self.options.HAVE_STL_GNU_CXX = True
      print "Gnu ext/ templates supported"
    except ExecutionError:
      self.options.HAVE_STL_GNU_CXX = False

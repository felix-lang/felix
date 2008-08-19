from fbuild.flxbuild.cxx_base import cxx_base
from fbuild.flxbuild.msvc_mixin import msvc_mixin

class msvcxx(msvc_mixin, cxx_base):
  # RF: this might be a good place to put the RTTI switch zzz
  DEFAULT_COM = 'cl'

  def check_options(self):
    msvc_mixin.check_options(self)
    cxx_base.check_options(self)

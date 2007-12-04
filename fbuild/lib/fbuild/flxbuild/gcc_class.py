from fbuild.flxbuild.c_base import c_base
from fbuild.flxbuild.gnu_mixin import gnu_mixin

class gcc(gnu_mixin, c_base):
  DEFAULT_COM = 'gcc'

  def check_options(self):
    c_base.check_options(self)
    gnu_mixin.check_options(self)

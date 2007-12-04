from fbuild.flxbuild.c_base import c_base
from fbuild.flxbuild.msvc_mixin import msvc_mixin

class msvcc(msvc_mixin, c_base):
  DEFAULT_COM = 'cl'

  def check_options(self):
    msvc_mixin.check_options(self)
    c_base.check_options(self)

  def compile_thing(self, COM, *args, **kwds):
    # make sure we tell the compiler to compile code as a c file
    return super(msvcc, self).compile_thing(COM + ' /TC', *args, **kwds)

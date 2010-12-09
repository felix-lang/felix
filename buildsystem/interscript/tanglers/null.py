#line 156 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
# null tangler
# NOTE: a null tangler is _not_ the same as
# some other tangler with a null sink:
# null tanglers do _not_ write to the weaver!
# Use a null tangler to remove files from the
# source _and_ documentation
from interscript.drivers.sinks.null import null_sink
from interscript.tanglers.base import tangler_base

class null_tangler(tangler_base):
  def __init__(self,weaver=None):
    tangler_base.__init__(self,null_sink(),weaver)
    self.language = 'None'

  def get_comment_tangler(self):
    return self

  def writeline(self,data,file,count,inhibit_sref=0):
    pass


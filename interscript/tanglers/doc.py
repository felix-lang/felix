#line 183 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
# document tangler: writes text as doco,
# doesn't generate a file. Not the same as 'no' tangler
from interscript.tanglers.base import tangler_base

class doc_tangler(tangler_base):
  def __init__(self,weaver):
    tangler_base.__init__(self,null_sink(),weaver)
    self.language = 'document'

  def get_comment_tangler(self):
    return self

  def writeline(self,data,file,count,inhibit_sref=0):
    self.weaver.writeline(data,file,count)


#line 492 "interscript/src/tanglers.ipk"

from interscript.tanglers.base import tangler_base
from interscript.tanglers.cpp import hash_comment_tangler

class tcl_tangler(tangler_base):
  def __init__(self,sink,weaver):
    tangler_base.__init__(self,sink,weaver)
    self.language = 'tcl'

  def write_comment(self,line):
    self._writeline('# '+line)

  def start_section(self, file, count):
    data = 'line '+str(count)+' '+'"'+file+'"'
    self._writeline(data)
    if self.weaver:
      self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return hash_comment_tangler(self.sink,weaver, '# ')

  def get_string_tangler(self,eol,width=0):
    # This is _wrong_ and needs to be fixed!
    return c_string_tangler(self.sink,self.weaver,eol,width)



#line 372 "interscript/src/tanglers.ipk"
from interscript.tanglers.base import tangler_base
from interscript.tanglers.cpp import cpp_comment_tangler
from interscript.tanglers.c import c_string_tangler
import re
class java_tangler(tangler_base):
  def __init__(self,sink,weaver):
    tangler_base.__init__(self,sink,weaver)
    self.matchid = re.compile('^[^A-Za-z_]*([A-Za-z_][A-Za-z_0-9]*)(.*)$')
    self.language = 'java'

  def write_comment(self,line):
    self._writeline('// '+line)

  def start_section(self, file, count):
    data = '//#line '+str(count)+' '+'"'+file+'"'
    self._writeline(data)
    if self.weaver:
      self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return cpp_comment_tangler(self.sink)

  def get_string_tangler(self,eol,width):
    return c_string_tangler(self.sink,self.weaver,eol,width)


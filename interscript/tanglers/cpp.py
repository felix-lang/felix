#line 311 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
# c++ tangler: write to a file, insert source line numbers
# using '#line' pre-processor directives

from interscript.tanglers.base import tangler_base
import re

class cpp_tangler(tangler_base):
  def __init__(self,sink,weaver,nosref=0):
    tangler_base.__init__(self,sink,weaver,nosref)
    self.matchid = re.compile('^[^A-Za-z_]*([A-Za-z_][A-Za-z_0-9]*)(.*)$')
    self.language = 'C++'

  def write_comment(self,line):
    self._writeline('// '+line)

  def start_section(self, file, count):
    data = '#line '+str(count)+' '+'"'+file+'"'
    self._writeline(data)
    if self.weaver:
      self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return cpp_comment_tangler(self.sink, weaver)

  def get_string_tangler(self,eol,width):
    return c_string_tangler(self.sink,self.weaver,eol,width)


#line 342 "interscript/src/tanglers.ipk"
class hash_comment_tangler(tangler_base):
  def __init__(self, writer, weaver, prefix):
    tangler_base.__init__(self,writer, weaver)
    self.prefix = prefix
    self.language = prefix+' comment'

  def writeline(self,data,file,count,inhibit_sref=0):
    self.weaver.writeline(data)
    self._writeline(self.prefix+line)

#line 354 "interscript/src/tanglers.ipk"
class cpp_comment_tangler(hash_comment_tangler):
  def __init__(self, writer,weaver):
    hash_comment_tangler.__init__(self,writer,weaver,'// ')


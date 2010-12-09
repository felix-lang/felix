#line 233 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
# c tangler: write to a file, insert source line numbers
# using '#line' pre-processor directives

from interscript.tanglers.base import tangler_base
import re
import string
class c_tangler(tangler_base):
  def __init__(self,sink,weaver,nosref=0):
    tangler_base.__init__(self,sink,weaver,nosref)
    self.matchid = re.compile('^[^A-Za-z_]*([A-Za-z_][A-Za-z_0-9]*)(.*)$')
    self.language = 'C'

  def write_comment(self,line,file,count):
    self.writeline('/* '+line+'*/')

  def start_section(self, file, count):
    data = '#line '+str(count)+' '+'"'+repr(file)[1:-1]+'"'
    self._writeline(data)
    if self.weaver:
      self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return c_comment_tangler(self.sink)

  def get_string_tangler(self,eol='\\n',width=0):
    return c_string_tangler(self.sink,self.weaver,eol,width)

#line 263 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
class c_comment_tangler(tangler_base):
  def __init__(self,sink,weaver):
    tangler_base.__init__(self,sink,weaver)

  def writeline(self,data,file,count,inhibit_sref=0):
    if self.count == 0:
      self._writeline('/* '+data)
    else:
      self._writeline(' * '+data)
    self.weaver.writeline(data)

  def __del__(self):
    self._writeline(' */')

#line 280 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
class c_string_tangler(tangler_base):
  def __init__(self,sink,weaver,eol,width):
    print('Initialising c string tangler, eol=',eol,'width=',width)
    tangler_base.__init__(self,sink,weaver)
    self.eol=eol
    self.width=width
    self.language = 'C string'

  def writeline(self,data,file,count,inhibit_sref=0):
    data = data.rstrip() # remove trailing spaces
    if self.width > 0: data = string.ljust(data, self.width)
    line = '"'
    for ch in data:
      if ch in '\\"': line = line + '\\' + ch
      else: line = line + ch
    line = line + self.eol + '"'
    self._writeline(line)
    self.weaver.writeline(data)


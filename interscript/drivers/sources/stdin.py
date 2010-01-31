#line 706 "interscript/src/source_drivers.ipk"
#---------------------------------------------------------
# gets input from _python_ sys.stdin object
# same as named_file_source, except named 'standard input'
# and doesn't close file on destruction
import sys
from interscript.drivers.sources.base import source
from interscript.drivers.sources.base import eof
import string

class stdin_source(source):
  def __init__(self, encoding='utf8',**kwds):
    source.__init__(*(self,encoding), **kwds)
    self.name = 'standard input'
    self.closed = 0

  def _raw_readline(self):
    if self.closed:
      raise eof
    line = sys.stdin.readline()
    if len(line)==0: raise eof
    self.lines_read = self.lines_read + 1
    return line.rstrip()


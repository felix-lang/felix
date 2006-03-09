#line 732 "interscript/src/source_drivers.ipk"
from interscript.drivers.sources.base import source
from interscript.drivers.sources.base import eof
import string

class cache_source(source):
  def __init__(self, name, master_frame, encoding='utf8'):
    source.__init__(self, encoding)
    self.name = name
    self.saved = ''
    macros = master_frame.get_persistent_frame('macros')
    self.list = macros.get(name,[])
    self.closed = 0

  def _raw_readline(self):
    if len(self.list)>self.lines_read:
      line = self.list[self.lines_read]
    else:
      raise eof
    self.lines_read = self.lines_read + 1
    return line

  def rewind_source(self):
    self.lines_read = 0


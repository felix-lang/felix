#line 16 "interscript/src/storage_drivers.ipk"
from interscript.drivers.sources.base import source
from interscript.drivers.sources.base import eof
from interscript.drivers.sinks.base import sink_base

class memory(source,sink_base):
  def __init__(self,name,**kwds):
    source.__init__(self)
    sink_base.__init__(self)
    self.name = name
    self.saved = ''
    self.list = []
    for k in kwds.keys():
      self.k = kwds[k]
    self.closed = 0

  def raw_readline(self):
    if len(self.list)>self.lines_read:
      line = self.list[self.lines_read]
    else:
      raise eof
    self.lines_read = self.lines_read + 1
    return line

  def raw_eol(self):
    self.list.append(self.saved)
    self.saved = ''

  def rewind_source(self):
    self.lines_read = 0

  def raw_write(self,data):
    self.saved = self.saved + data

